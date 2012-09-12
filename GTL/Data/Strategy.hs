{-# LANGUAGE ScopedTypeVariables #-}

module GTL.Data.Strategy ( StrategyXA, DeterministicStrategyXA
                         , randomizeStrategyXA, deterministicStrategiesXA
                         , StrategyXZA, DeterministicStrategyXZA
                         , randomizeStrategyXZA, deterministicStrategiesXZA ) where

import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (certainly)
import Data.Ix (Ix)
import GTL.Data.Finite (finiteFunctions)
import Data.Tuple.Curry (curryN)

-- Regular
type StrategyXA x a = x -> Dist a
type DeterministicStrategyXA x a = x -> a

randomizeStrategyXA :: DeterministicStrategyXA x a -> StrategyXA x a
randomizeStrategyXA = (certainly .)

deterministicStrategiesXA :: (Bounded x, Ix x, Bounded a, Ix a) => [DeterministicStrategyXA x a]
deterministicStrategiesXA = finiteFunctions

-- With Mockup State
type StrategyXZA x z a = x -> z -> Dist a
type DeterministicStrategyXZA x z a = x -> z -> a

randomizeStrategyXZA :: DeterministicStrategyXZA x z a -> StrategyXZA x z a
randomizeStrategyXZA strat x z = certainly $ strat x z

deterministicStrategiesXZA :: forall x z a. (Bounded x, Ix x, Bounded z, Ix z, Bounded a, Ix a) => [DeterministicStrategyXZA x z a]
deterministicStrategiesXZA = map curryN (finiteFunctions :: [(x, z) -> a])
