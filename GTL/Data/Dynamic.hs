module GTL.Data.Dynamic ( DynamicXA, DynamicxA, noStateDynamicxA
                        , DynamicXAS, DynamicxAS, noStateDynamicxAS ) where

import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (certainly)

-- Regular
type DynamicXA x a = x -> a -> Dist x
type DynamicxA a = DynamicXA () a

noStateDynamicxA :: DynamicxA a
noStateDynamicxA () _ = certainly ()

-- With Signal
type DynamicXAS x a s = x -> a -> s -> Dist x
type DynamicxAS a s = DynamicXAS () a s

noStateDynamicxAS :: DynamicxAS a s
noStateDynamicxAS () _ _ = certainly ()
