{-# LANGUAGE ScopedTypeVariables #-}

module GTL.Interaction.SimulatedLearning2 where

import GTL.Interaction.SimulatedConsistency2 (simulatedMockups2)
import GTL.Interaction.Optimality (uniformSmoothedOptimalResponse)
import GTL.Data.Mockup (Role, updateMockup, MockupArray, arrayToMockup, mockupToArray)
import GTL.Data.Utility (Epsilon, Unitless)
import GTL.Data.Signaling (SignalingWXAS2)
import GTL.Data.Finite (rangeF)
import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (uniform)
import Numeric.Probability.Random (runSeed, pick)

import Data.Ix (Ix)
import GTL.Data.History (History)
import Control.Monad.RWS (RWS, evalRWS, ask, get, put, tell)
import System.Random (StdGen, next)

type Time = Int

invProp :: Time -> Unitless
invProp = (**0.75) . (1.0/) . fromIntegral

data Param w x1 a1 s1 x2 a2 s2 = Param { role1     :: Role x1 a1 s1
                                       , role2     :: Role x2 a2 s2
                                       , signaling :: SignalingWXAS2 w x1 a1 s1 x2 a2 s2
                                       , epsilon1  :: Epsilon
                                       , epsilon2  :: Epsilon
                                       , stepSize1 :: Time -> Unitless
                                       , stepSize2 :: Time -> Unitless
                                       , timeMax   :: Time }

data Var a1 s1 h1 h1' a2 s2 h2 h2' = Var { mockupArray1 :: !(MockupArray a1 s1 h1 h1')
                                         , mockupArray2 :: !(MockupArray a2 s2 h2 h2')
                                         , generator    :: StdGen
                                         , time         :: Time } deriving (Show)

type Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = RWS (Param w x1 a1 s1 x2 a2 s2) [Var a1 s1 h1 h1' a2 s2 h2 h2'] (Var a1 s1 h1 h1' a2 s2 h2 h2')

runComp :: Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' comp -> Param w x1 a1 s1 x2 a2 s2 -> Var a1 s1 h1 h1' a2 s2 h2 h2' -> [Var a1 s1 h1 h1' a2 s2 h2 h2']
runComp comp param var = snd $ evalRWS comp param var

oneStep :: ( Bounded (h2' s2), Bounded (h2 a2), Bounded (h1' s1), Bounded (h1 a1)
           , Bounded s2, Bounded a2, Bounded s1, Bounded a1, Bounded x2, Bounded x1, Bounded w
           , Ix (h2' s2), Ix (h2 a2), Ix (h1' s1), Ix (h1 a1)
           , Ix s2, Ix a2, Ix s1, Ix a1, Ix x2, Ix x1, Ix w
           , History h2', History h2, History h1', History h1
           ) => (Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2') ()
oneStep = do
  Param { role1     = rol1
        , role2     = rol2
        , signaling = sig
        , epsilon1  = eps1
        , epsilon2  = eps2
        , stepSize1 = step1
        , stepSize2 = step2 } <- ask
  Var { mockupArray1 = arr1
      , mockupArray2 = arr2
      , generator    = gen
      , time         = t } <- get
  let t'               = t + 1
      mock1            = arrayToMockup arr1
      mock2            = arrayToMockup arr2
      resp1            = uniformSmoothedOptimalResponse rol1 eps1 mock1
      resp2            = uniformSmoothedOptimalResponse rol2 eps2 mock2
      (initStat, gen') = pickRand gen
      (mock1', mock2') = simulatedMockups2 rol1 rol2 sig resp1 resp2 gen' initStat
      mock1''          = updateMockup (step1 t') mock1 mock1'
      mock2''          = updateMockup (step2 t') mock2 mock2'
      arr1'            = mockupToArray mock1''
      arr2'            = mockupToArray mock2''
      (_, gen'')       = next gen'
      var              = Var arr1' arr2' gen'' t'
  put var

logVar :: Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' ()
logVar = get >>= (tell . (:[]))

pickRand :: forall a. (Bounded a, Ix a) => StdGen -> (a, StdGen)
pickRand gen = (runSeed gen $ pick $ (uniform rangeF :: Dist a), gen')
    where (_, gen') = next gen
