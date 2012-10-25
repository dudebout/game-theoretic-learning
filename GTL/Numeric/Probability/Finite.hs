{-# LANGUAGE FlexibleInstances #-}

module GTL.Numeric.Probability.Finite (toList, fromList, toLists, toMatrix, toVector, fromVector, almostCertainly) where

import GTL.Data.Unitless
import GTL.Numeric.Probability (Dist, Trans, Proba, (?!))
import Numeric.Probability.Distribution (relative, certainly)
import Numeric.LinearAlgebra hiding (toList, toLists, fromList, fromLists, (<>))
import qualified Numeric.LinearAlgebra as Lin (toList, fromList, fromLists)
import Data.Ix (Ix)
import GTL.Data.Finite (rangeF)

-- |Creates a distribution with all the weight but epsilon on a given element.
almostCertainly :: (Ix a, Bounded a) => Unitless -> a -> Dist a
almostCertainly epsi a = fromList $ map replacements initial
    where initial = toList $ certainly a
          replacements 1 = 1 - epsi
          replacements _ = epsi / (fromIntegral (length initial) - 1)

-- |Gives the list of probabilities for a finite distribution.
toList :: (Ix a, Bounded a, Eq a) => Dist a -> [Proba]
toList d = map (d ?!) rangeF

-- |Generates a finite probability distribution from a list of probabilities.
fromList :: (Ix a, Bounded a) => [Proba] -> Dist a
fromList = flip relative rangeF

-- |Gives the list of list of probabilities for a finite transition.
toLists :: (Ix a, Bounded a, Eq a) => Trans a -> [[Proba]]
toLists t = map (toList . t) rangeF

-- |Goes from a transition probability to a 'Matrix'.
toMatrix :: (Bounded a, Ix a, Eq a) => Trans a -> Matrix Proba
toMatrix = Lin.fromLists . toLists

-- |Creates a 'Vector' from a distribution.
toVector :: (Bounded a, Ix a) => Dist a -> Vector Proba
toVector = Lin.fromList . toList

-- |Creates a distribution from a 'Vector'.
fromVector :: (Bounded a, Ix a) => Vector Proba -> Dist a
fromVector = fromList . Lin.toList
