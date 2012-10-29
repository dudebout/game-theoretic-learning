module GTL.Data.Utility ( Value, Unitless, Discount, Epsilon, UtilityXA
                        , UtilityXAS, UtilityxAS, UtilityAS, toUtilityAS, toUtilityxAS) where

import GTL.Data.Unitless (Unitless)

type Value    = Double
type Discount = Unitless
type Epsilon  = Value

-- State Action and Signal
type UtilityXAS x a s = x -> a -> s -> Value

-- No Signal
type UtilityXA x a = x -> a -> Value

-- No State
type UtilityxAS a s = UtilityXAS () a s
type UtilityAS a s = a -> s -> Value

toUtilityAS :: UtilityxAS a s -> UtilityAS a s
toUtilityAS util a s = util () a s

toUtilityxAS :: UtilityAS a s -> UtilityxAS a s
toUtilityxAS util () a s = util a s
