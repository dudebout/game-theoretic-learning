-- |This module computes the stationary distribution of a finite ergodic Markov chains.
module GTL.Numeric.MarkovChain.Ergodic (ergodicStationary) where

import GTL.Numeric.Probability (Dist, Trans)
import GTL.Numeric.Probability.Finite (fromVector, toMatrix)
import Data.Ix (Ix)
import Data.Maybe (mapMaybe)
import Numeric.LinearAlgebra

-- |Computes the stationary distribution of a finite ergodic Markov chains
-- described by its transition function.
ergodicStationary :: (Ix a, Bounded a, Eq a) => Trans a -> Dist a
ergodicStationary = fromVector . unique . realEigenvectors1 . toMatrix

-- |Extracts the unique vector contained in a list and throws and error if it is not unique.
unique :: [Vector Double] -> Vector Double
unique vs | num == 1  = head vs
          | otherwise = error $ "An ergodic transition matrix MUST have a UNIQUE stationary distribution. Found: " ++ show num
          where num = length vs

complexTolerance :: Double
complexTolerance = 100 * eps

vectorDoubleTolerance :: Double
vectorDoubleTolerance = 100 * eps

-- |Computes the list of real eigenvectors associated with eigenvalue 1.
realEigenvectors1 :: Matrix Double -> [Vector Double]
realEigenvectors1 m = verifyEigenvector1 $ keepOnlyReals listEigenvectors1
    where verifyEigenvector1 = filter (\x -> approx vectorDoubleTolerance x (m' <> x))
          keepOnlyReals      = mapMaybe realVector
          listEigenvectors1  = eigenvectors1 complexTolerance m'
          m' = trans m

-- |Computes the list of eigenvectors associated with eigenvalue 1.
eigenvectors1 :: Double -> Matrix Double -> [Vector (Complex Double)]
eigenvectors1 t m = [extractColumn v idx | idx <- find (approx1 t) s]
    where (s, v) = eig m

-- |Determines if a complex number is almost equal to 1.
approx1 :: Double -> Complex Double -> Bool
approx1 tol x = magnitude (x - 1) <= tol

-- |Determines if two vectors are almost equal to each other.
approx :: Double -> Vector Double -> Vector Double -> Bool
approx tol u v = normInf (u - v) < tol

{- |Extracts a column from a matrix.

The column is specified by its index.
-}
extractColumn :: Element b => Matrix b -> Int -> Vector b
extractColumn m idx = flatten $ takeColumns 1 $ dropColumns idx m

-- |Returns 'Just' a vector if it is real, and 'Nothing' else.
realVector :: Vector (Complex Double) -> Maybe (Vector Double)
realVector z | normInf y == 0 = Just x
             | otherwise      = Nothing
             where (x, y) = fromComplex z
