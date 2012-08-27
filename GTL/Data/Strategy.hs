{-# LANGUAGE ScopedTypeVariables #-}

module GTL.Data.Strategy ( StrategyXA, DeterministicStrategyXA, deterministicStrategiesXA
                         , StrategyXYA, DeterministicStrategyXYA, deterministicStrategiesXYA ) where

import GTL.Numeric.Probability (Dist)
import Data.Ix (Ix)
import GTL.Data.Finite (finiteFunctions)
import Data.Tuple.Curry (curryN)

-- Regular
type StrategyXA x a = x -> Dist a
type DeterministicStrategyXA x a = x -> a

deterministicStrategiesXA :: (Bounded x, Ix x, Bounded a, Ix a) => [DeterministicStrategyXA x a]
deterministicStrategiesXA = finiteFunctions

-- With Mockup State
type StrategyXYA x y a = x -> y -> Dist a
type DeterministicStrategyXYA x y a = x -> y -> a

deterministicStrategiesXYA :: forall x y a. (Bounded x, Ix x, Bounded y, Ix y, Bounded a, Ix a) => [DeterministicStrategyXYA x y a]
deterministicStrategiesXYA = map curryN (finiteFunctions :: [(x, y) -> a])
