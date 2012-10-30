module GTL.Numeric.Probability.EmpiricalFrequency ( addObservation
                                                  , empiricalFrequencyStepSize
                                                  , computeEmpiricalFrequencies ) where

import Control.Monad.State
import Control.Monad.Loops (whileM)
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

oneStep :: Ord a => State (Accumulator a) (Dist a)
oneStep = do
  Accumulator { observations = observs
              , time         = t
              , distribution = dist } <- get
  let obs = head observs
      t'  = t + 1
      d'  = addObservation dist obs $ empiricalFrequencyStepSize t'
  put $ Accumulator (tail observs) t' d'
  return d'

notDoneM :: State (Accumulator a) Bool
notDoneM = fmap notDone get

notDone :: Accumulator a -> Bool
notDone = not . null . observations

complete :: Ord a => State (Accumulator a) [Dist a]
complete = whileM notDoneM oneStep

computeEmpiricalFrequencies :: Ord a => [a] -> [Dist a]
computeEmpiricalFrequencies []       = []
computeEmpiricalFrequencies (a:as) = certainly a:(evalState complete $ Accumulator as 1 (certainly a))
