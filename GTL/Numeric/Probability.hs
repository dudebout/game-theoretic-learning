module GTL.Numeric.Probability ( Proba, Dist, Trans, (>>=$), (?!)
                               , joint2, joint3, joint4, joint5 ) where

import Numeric.Probability.Distribution (T, just, (??))
import qualified Numeric.Probability.Transition as Transition (T)
import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import GTL.Data.Unitless (ZeroOne)

-- |Using probabilities on Double because it is optimized.
type Proba = ZeroOne
-- |Probability distribution.
type Dist = T Proba
-- |Probability transition.
type Trans a = Transition.T Proba a

-- |Applies a regular function to a distribution.
(>>=$) :: Functor f => f a -> (a -> b) -> f b
(>>=$) = flip fmap
infixl 1 >>=$

-- |Computes the probability of a certain element in a distribution.
(?!) :: Eq a => Dist a -> a -> Proba
d ?! x = just x ?? d

-- |Computes the joint distribution of two independent random variables.
joint2 :: Dist a -> Dist b -> Dist (a, b)
joint2 = liftM2 (,)

-- |Computes the joint distribution of three independent random variables.
joint3 :: Dist a -> Dist b -> Dist c -> Dist (a, b, c)
joint3 = liftM3 (,,)

-- |Computes the joint distribution of four independent random variables.
joint4 :: Dist a -> Dist b -> Dist c -> Dist d -> Dist (a, b, c, d)
joint4 = liftM4 (,,,)

-- |Computes the joint distribution of five independent random variables.
joint5 :: Dist a -> Dist b -> Dist c -> Dist d -> Dist e -> Dist (a, b, c, d, e)
joint5 = liftM5 (,,,,)
