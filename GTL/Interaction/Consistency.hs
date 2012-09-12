{-# LANGUAGE FlexibleContexts #-}

module GTL.Interaction.Consistency (mockup) where

import GTL.Data.Dynamic (DynamicXAS)
import GTL.Data.Signaling (SignalingWXAS)
import GTL.Numeric.MarkovChain.Ergodic (ergodicStationary)

import GTL.Data.History (History)
import GTL.Data.Mockup (Mockup, MockupState, updateMockupState, MockupStrategy, Role(..))

import Data.Ix (Ix)
import Data.Tuple.Curry (uncurryN)
import GTL.Numeric.Probability (Trans, (>>=$))
import Numeric.Probability.Distribution ((>>=?), just, norm, certainly)

data System w x a s h h' = System { signaling :: SignalingWXAS w x a s
                                  , dynamic   :: DynamicXAS x a s
                                  , strategy  :: MockupStrategy x a s h h' }

data JointState w x a s h h' = JointState { worldState  :: w
                                          , state       :: x
                                          , mockupState :: MockupState a s h h' } deriving (Bounded, Eq, Ix, Ord, Show)


mockup :: ( Bounded (h' s), Bounded (h a), Ix (h' s), Ix (h a), History h', History h
          , Bounded s, Bounded a, Bounded x, Bounded w, Ix s, Ix a, Ix x, Ix w
          ) => Role x a s -> SignalingWXAS w x a s
       -> MockupStrategy x a s h h' -> Mockup a s h h'
mockup rol sig strat = systemToMockup $ toSystem rol sig strat

systemToMockup :: ( Bounded w, Ix w, Bounded x, Ix x
                  , Bounded a, Ix a, Bounded s, Ix s
                  , Bounded (h a), Ix (h a), Bounded (h' s), Ix (h' s)
                  , History h, History h'
                  ) => System w x a s h h' -> Mockup a s h h'
systemToMockup syst = mock
    where
      mock z   = norm $ statDist >>=? condit z >>= computeStatesAndAction >>= sig >>=$ extractSignal
      statDist = ergodicStationary $ transition syst
      condit z = just z . mockupState
      sig      = uncurryN $ signaling syst
      strat    = strategy syst
      computeStatesAndAction joint = do
        let w = worldState joint
        let x = state joint
        a <- strat x (mockupState joint)
        return (w, x, a)
      extractSignal (_, s) = s

toSystem :: Role x a s -> SignalingWXAS w x a s
         -> MockupStrategy x a s h h' -> System w x a s h h'
toSystem rol sig = System sig dyn
    where dyn = augmentedDynamic rol

transition :: (History h, History h') =>
               System w x a s h h'
            -> Trans (JointState w x a s h h')
transition syst (JointState w x z) = do
  a'       <- strat x z
  (w', s') <- signal w x a'
  z'       <- certainly $ updateMockupState z a' s'
  x'       <- dyn x a' s'
  return $ JointState w' x' z'
      where (System signal dyn strat) = syst
