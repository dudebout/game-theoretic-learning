module GTL.Data.Utility ( Value, Unitless, Discount, Epsilon, UtilityXA
                        , UtilityXAS, UtilityxAS, toUtilityxAS
                        , UtilityAS, toUtilityAS, bestResponseAS ) where

import Data.Ix (Ix)
import Numeric.Probability.Distribution (expected)
import qualified Numeric.Probability.Distribution as P (map)
import GTL.Numeric.Probability (Dist)
import GTL.Data.Unitless (Unitless)
import GTL.Data.Finite (argmax)

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

bestResponseAS :: (Bounded a, Ix a, Bounded s, Ix s) => UtilityAS a s -> Dist s -> a
bestResponseAS u ds = argmax $ \a -> expected $ P.map (u a) ds
