{-# LANGUAGE FlexibleContexts #-}

module GTL.Interaction.Consistency2 (mockups2) where

import GTL.Data.Dynamic (DynamicXAS)
import GTL.Data.Signaling (SignalingWXAS2)
import GTL.Numeric.MarkovChain.Ergodic (ergodicStationary)

import GTL.Data.History (History)
import GTL.Data.Mockup (Mockup, MockupState, addToMockupState, MockupStrategy, Role(..))

import Data.Ix (Ix)
import Data.Tuple.Curry (uncurryN)
import GTL.Numeric.Probability (Trans, (>>=$))
import Numeric.Probability.Distribution ((>>=?), just, norm, certainly)

data System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = System2 { signaling2 :: SignalingWXAS2 w x1 a1 s1 x2 a2 s2
                                                         , dynamics1  :: DynamicXAS x1 a1 s1
                                                         , dynamics2  :: DynamicXAS x2 a2 s2
                                                         , strategy1  :: MockupStrategy x1 a1 s1 h1 h1'
                                                         , strategy2  :: MockupStrategy x2 a2 s2 h2 h2' }

data JointState2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = JointState2 { worldState   :: w
                                                                 , state1       :: x1
                                                                 , mockupState1 :: MockupState a1 s1 h1 h1'
                                                                 , state2       :: x2
                                                                 , mockupState2 :: MockupState a2 s2 h2 h2' } deriving (Bounded, Eq, Ix, Ord, Show)


mockups2 :: ( Bounded (h2' s2), Bounded (h2 a2), Bounded (h1' s1), Bounded (h1 a1)
            , Bounded s2, Bounded a2, Bounded s1, Bounded a1, Bounded x2, Bounded x1, Bounded w
            , Ix (h2' s2), Ix (h2 a2), Ix (h1' s1), Ix (h1 a1)
            , Ix s2, Ix a2, Ix s1, Ix a1, Ix x2, Ix x1, Ix w
            , History h2', History h2, History h1', History h1
            ) => Role x1 a1 s1 -> Role x2 a2 s2 -> SignalingWXAS2 w x1 a1 s1 x2 a2 s2
         -> MockupStrategy x1 a1 s1 h1 h1' -> MockupStrategy x2 a2 s2 h2 h2'
         -> (Mockup a1 s1 h1 h1', Mockup a2 s2 h2 h2')
mockups2 rol1 rol2 sig strat1 strat2 = systemToMockups2 $ toSystem2 rol1 rol2 sig strat1 strat2

systemToMockups2 :: ( Bounded w, Ix w, Bounded x1, Ix x1, Bounded x2, Ix x2
                    , Bounded a1, Ix a1, Bounded s1, Ix s1
                    , Bounded a2, Ix a2, Bounded s2, Ix s2
                    , Bounded (h1 a1), Ix (h1 a1), Bounded (h1' s1), Ix (h1' s1)
                    , Bounded (h2 a2), Ix (h2 a2), Bounded (h2' s2), Ix (h2' s2)
                    , History h1, History h1'
                    , History h2, History h2'
                    ) => System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' -> (Mockup a1 s1 h1 h1', Mockup a2 s2 h2 h2')
systemToMockups2 syst = (pred1, pred2)
    where
      pred1 y1   = norm $ statDist >>=? condit1 y1 >>= computeStatesAndActions >>= sig >>=$ extractSignal1
      pred2 y2   = norm $ statDist >>=? condit2 y2 >>= computeStatesAndActions >>= sig >>=$ extractSignal2
      statDist   = ergodicStationary $ transition2 syst
      condit1 y1 = just y1 . mockupState1
      condit2 y2 = just y2 . mockupState2
      sig        = uncurryN $ signaling2 syst
      strat1     = strategy1 syst
      strat2     = strategy2 syst
      computeStatesAndActions j = do
        let w  = worldState j
        let x1 = state1 j
        let x2 = state2 j
        a1 <- strat1 x1 (mockupState1 j)
        a2 <- strat2 x2 (mockupState2 j)
        return (w, x1, a1, x2, a2)
      extractSignal1 (_, s, _) = s
      extractSignal2 (_, _, s) = s

toSystem2 :: Role x1 a1 s1 -> Role x2 a2 s2 -> SignalingWXAS2 w x1 a1 s1 x2 a2 s2
          -> MockupStrategy x1 a1 s1 h1 h1' -> MockupStrategy x2 a2 s2 h2 h2'
          -> System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2'
toSystem2 rol1 rol2 sig strat1 strat2 = System2 sig dyn1 dyn2 strat1 strat2
    where dyn1 = augmentedDynamic rol1
          dyn2 = augmentedDynamic rol2

transition2 :: (History h1, History h1', History h2, History h2') =>
               System2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2'
            -> Trans (JointState2 w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2')
transition2 syst (JointState2 w x1 y1 x2 y2) = do
  a1'            <- strat1 x1 y1
  a2'            <- strat2 x2 y2
  (w', s1', s2') <- signal w x1 a1' x2 a2'
  y1'            <- certainly $ addToMockupState y1 a1' s1'
  y2'            <- certainly $ addToMockupState y2 a2' s2'
  x1'            <- dyn1 x1 a1' s1'
  x2'            <- dyn2 x2 a2' s2'
  return $ JointState2 w' x1' y1' x2' y2'
      where (System2 signal dyn1 dyn2 strat1 strat2) = syst
