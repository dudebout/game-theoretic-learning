{-# LANGUAGE FlexibleContexts #-}

module GTL.Modeling.FullGame where

import GTL.Data.Finite (Ix(..))
import Data.Tuple.Curry (uncurryN)
import Numeric.Probability.Distribution ((>>=?), just, norm, certainly)
import GTL.Numeric.MarkovChain.Ergodic (ergodicStationary)
import GTL.Modeling.ModelState (ModelState, addToModelState)
import GTL.Numeric.Probability (Dist, Trans, (>>=$))
import GTL.Data.History (History)

type Strategy x a s h h' = x -> ModelState a s h h' -> Dist a
-- Maybe add the state to the predictor
type Predictor a s h h' = ModelState a s h h' -> Dist s
type Dynamics x a s = x -> a -> s -> Dist x

type Signaling2 w x1 a1 s1 x2 a2 s2 = w -> x1 -> a1 -> x2 -> a2 -> Dist (w, s1, s2)

data System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' =
    System2 { signaling2 :: Signaling2 w x1 a1 s1 x2 a2 s2
            , dynamics1  :: Dynamics x1 a1 s1
            , dynamics2  :: Dynamics x2 a2 s2
            , strategy1  :: Strategy x1 a1 s1 h1 h1'
            , strategy2  :: Strategy x2 a2 s2 h2 h2' }

data JointState2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = JointState2 { worldState  :: w
                                                                 , state1      :: x1
                                                                 , modelState1 :: ModelState a1 s1 h1 h1'
                                                                 , state2      :: x2
                                                                 , modelState2 :: ModelState a2 s2 h2 h2' } deriving (Show, Eq, Ord, Bounded, Ix)

transition2 :: (History h1, History h1', History h2, History h2') =>
               System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2'
            -> Trans (JointState2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2')
transition2 syst (JointState2 w x1 y1 x2 y2) = do
  a1' <- strat1 x1 y1
  a2' <- strat2 x2 y2
  (w', s1', s2') <- signal w x1 a1' x2 a2'
  y1' <- certainly $ addToModelState y1 a1' s1'
  y2' <- certainly $ addToModelState y2 a2' s2'
  x1' <- dyn1 x1 a1' s1'
  x2' <- dyn2 x2 a2' s2'
  return $ JointState2 w' x1' y1' x2' y2'
      where (System2 signal dyn1 dyn2 strat1 strat2) = syst

models2 :: ( Bounded w, Ix w, Bounded x1, Ix x1, Bounded x2, Ix x2
           , Bounded a1, Ix a1, Bounded s1, Ix s1
           , Bounded a2, Ix a2, Bounded s2, Ix s2
           , Bounded (h1 a1), Ix (h1 a1), Bounded (h1' s1), Ix (h1' s1)
           , Bounded (h2 a2), Ix (h2 a2), Bounded (h2' s2), Ix (h2' s2)
           , History h1, History h1'
           , History h2, History h2' ) =>
           System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2'
        -> (Predictor a1 s1 h1 h1', Predictor a2 s2 h2 h2')
models2 syst = (pred1, pred2)
    where
      pred1 y1 = norm $ statDist >>=? condit1 y1 >>= computeStatesAndActions >>= sig >>=$ extractSignal1
      pred2 y2 = norm $ statDist >>=? condit2 y2 >>= computeStatesAndActions >>= sig >>=$ extractSignal2
      statDist = ergodicStationary $ transition2 syst
      condit1 y1 = just y1 . modelState1
      condit2 y2 = just y2 . modelState2
      sig = uncurryN $ signaling2 syst
      strat1 = strategy1 syst
      strat2 = strategy2 syst
      computeStatesAndActions j = do
        let w = worldState j
        let x1 = state1 j
        let x2 = state2 j
        a1 <- strat1 x1 (modelState1 j)
        a2 <- strat2 x2 (modelState2 j)
        return (w, x1, a1, x2, a2)
      extractSignal1 (_, s, _) = s
      extractSignal2 (_, _, s) = s

type StatelessSignaling2 x1 a1 s1 x2 a2 s2 = x1 -> a1 -> x2 -> a2 -> Dist (s1, s2)

toSignaling2 :: StatelessSignaling2 x1 a1 s1 x2 a2 s2 -> Signaling2 () x1 a1 s1 x2 a2 s2
toSignaling2 signaling () x1 a1 x2 a2 = do
  (s1, s2) <- signaling x1 a1 x2 a2
  return ((), s1, s2)
