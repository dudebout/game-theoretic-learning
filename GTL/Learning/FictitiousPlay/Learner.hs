{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GTL.Learning.FictitiousPlay.Learner
    ( FPState(..), initialFPState
    , FPLearner, FPLearnerR
    , classicFPLearner, classicFPLearnerR
    ) where

import Control.Monad.RWS
import Data.Ix (Ix)
import GTL.Numeric.Probability (Dist)
import GTL.Numeric.Probability.EmpiricalFrequency (addObservation, empiricalFrequencyStepSize)
import GTL.Data.Utility (bestResponseAS)
import GTL.Data.Time (Time)
import GTL.Data.Unitless (StepSize)
import GTL.Learning.OneShot

data FPState s = FPState { distrib :: Dist s
                         , time    :: Time }

data Param = Param { step :: Time -> StepSize }

reportFPState :: RWS r [FPState s] (FPState s) ()
reportFPState = get >>= tell . (:[])

accumulateSignal :: (Ord s, Monoid w) => s -> RWS Param w (FPState s) ()
accumulateSignal s = do
  FPState { distrib  = sd
          , time     = t } <- get
  st <- asks step
  let t' = t + 1
      sd' = addObservation sd s (st t')
  put $ FPState sd' t'

generateAction :: (Bounded a, Ix a, Bounded s, Ix s, Monoid w) => Role a s -> RWS Param w (FPState s) a
generateAction r = do
  sd <- gets distrib
  let u = utility r
  return $ bestResponseAS u sd

initialFPState :: Dist s -> FPState s
initialFPState sd = FPState sd 0

classicParam :: Param
classicParam = Param empiricalFrequencyStepSize

mainStep :: (Bounded a, Ix a, Bounded s, Ix s, Monoid w) => Role a s -> s -> RWS Param w (FPState s) a
mainStep r s = accumulateSignal s >> generateAction r

mainStepWithReport :: (Bounded a, Ix a, Bounded s, Ix s) => Role a s -> s -> RWS Param [FPState s] (FPState s) a
mainStepWithReport r s = do
  a <- mainStep r s
  reportFPState
  return a

makeClassicLearner :: (Monoid w) => (Role a s -> s -> RWS Param w (FPState s) a) -> Learner (FPState s) a s w
makeClassicLearner rwsStep = Learner theStep
    where theStep r stat s = runRWS (rwsStep r s) classicParam stat

type FPLearner a s w = Learner (FPState s) a s w
type FPLearnerR a s = FPLearner a s [FPState s]

classicFPLearner :: (Bounded a, Ix a, Bounded s, Ix s, Monoid w) => FPLearner a s w
classicFPLearner = makeClassicLearner mainStep

classicFPLearnerR :: (Bounded a, Ix a, Bounded s, Ix s) => FPLearnerR a s
classicFPLearnerR = makeClassicLearner mainStepWithReport
