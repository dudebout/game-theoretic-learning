module GTL.Numeric.Probability.EmpiricalFrequency ( addObservation
                                                  , empiricalFrequencyStepSize
                                                  , computeEmpiricalFrequencies ) where

import Control.Monad.RWS
import Control.Monad.Loops (whileM_)
import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (relative, certainly, unfold)
import GTL.Data.Unitless (StepSize)
import GTL.Data.Time (Time)

addObservation :: Ord a => Dist a -> a -> StepSize -> Dist a
addObservation dist obs step = unfold $ relative [1 - step, step] [dist, certainly obs]

empiricalFrequencyStepSize :: Time -> StepSize
empiricalFrequencyStepSize = (1 /) . fromIntegral

data Accumulator a = Accumulator { observations :: [a]
                                 , time         :: Time
                                 , distribution :: Dist a }

accumulate :: (Ord a, Monoid w) => RWS () w (Accumulator a) ()
accumulate = do
  Accumulator { observations = observs
              , time         = t
              , distribution = dist } <- get
  let obs = head observs
      d'  = addObservation dist obs $ empiricalFrequencyStepSize t
  put $ Accumulator (tail observs) (t + 1) d'

reportDist :: RWS () [Dist a] (Accumulator a) ()
reportDist = gets distribution >>= tell . (:[])

notDoneM :: (Monoid w) => RWS () w (Accumulator a) Bool
notDoneM = fmap notDone get

notDone :: Accumulator a -> Bool
notDone = not . null . observations

oneStep :: Ord a => RWS () [Dist a] (Accumulator a) ()
oneStep = accumulate >> reportDist

complete :: Ord a => RWS () [Dist a] (Accumulator a) ()
complete = whileM_ notDoneM oneStep

computeEmpiricalFrequencies :: Ord a => [a] -> [Dist a]
computeEmpiricalFrequencies []         = []
computeEmpiricalFrequencies as@(a:_) = snd $ evalRWS complete () $ Accumulator as 1 (certainly $ a)
