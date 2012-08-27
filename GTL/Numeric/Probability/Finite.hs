{-# LANGUAGE FlexibleInstances #-}

module GTL.Numeric.Probability.Finite (toList, fromList, toLists, toMatrix, fromVector) where

import GTL.Numeric.Probability (Dist, Trans, Proba, (?!))
import Numeric.Probability.Distribution (norm, relative, decons)
import Numeric.LinearAlgebra hiding (toList, toLists, fromList, fromLists, (<>))
import qualified Numeric.LinearAlgebra as Lin (toList, fromLists)
import Data.Ix (Ix)
import GTL.Data.Finite (rangeF)
import Text.PrettyPrint.Leijen
import Text.Printf (printf)

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

-- |Creates a distribution from a 'Vector'.
fromVector :: (Bounded a, Ix a) => Vector Proba -> Dist a
fromVector = fromList . Lin.toList

trunc:: Int -> Double -> Doc
trunc precision = text . printf ("%." ++ show precision ++ "f")

distrib :: Doc
distrib = text "~"

instance (Ix a, Bounded a, Pretty a, Ord a) => Pretty (Dist a) where
    pretty = probaList . map (\(x, p) -> probaPair [pretty x, trunc 4 p]) . decons . norm
        where probaPair = encloseSep empty empty distrib
              probaList = encloseSep  (lbrace <> space) (space <> rbrace) (semi <> space)
