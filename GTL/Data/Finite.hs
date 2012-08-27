{-# LANGUAGE ScopedTypeVariables #-}

module GTL.Data.Finite ( rangeF, indexF, indexF2, indexF3, indexF2x2, indexF3x3
                       , indexI, indexI2, indexI3, ixmapI, ixmapI2, ixmapI3
                       , ixmapF, ixmapF2, ixmapF3, functionF, functionF2, functionF3
                       , listI, listsI, vectorI, matrixI, finiteFunctions
                       , cartesianProduct2 ) where

import Data.Ix (Ix(..))
import Numeric.LinearAlgebra hiding (toList, toLists)
import qualified Numeric.LinearAlgebra as Lin (toList, toLists)
import Control.Monad (liftM2)
import Control.Arrow ((***))
import Control.Applicative ((<$>), (<*>))
import Data.Array.IArray (IArray, array, bounds, ixmap)
import Foreign.Storable (Storable)

-- | Suffix F: the input is Finite (i.e. Ix and Bounded data) indexed,
-- Suffix I: the input is Int indexed

boundsF :: (Bounded a) => (a, a)
boundsF = (minBound, maxBound)

rangeF :: (Ix a, Bounded a) => [a]
rangeF = range boundsF

indexF :: (Ix a, Bounded a) => a -> Int
indexF = index boundsF

indexF2 :: (Ix a, Bounded a, Ix b, Bounded b) => (a, b) -> (Int, Int)
indexF2 = indexF *** indexF

indexF3 :: (Ix a, Bounded a, Ix b, Bounded b, Ix c, Bounded c) => (a, b, c) -> (Int, Int, Int)
indexF3 (x, y, z) = (indexF x, indexF y, indexF z)

indexF2x2 :: (Ix a, Bounded a, Ix b, Bounded b) => ((a, b), (a, b)) -> ((Int, Int), (Int, Int))
indexF2x2 = indexF2 *** indexF2

indexF3x3 :: (Ix a, Bounded a, Ix b, Bounded b, Ix c, Bounded c) => ((a, b, c), (a, b, c)) -> ((Int, Int, Int), (Int, Int, Int))
indexF3x3 = indexF3 *** indexF3

indexI :: (Ix a, Bounded a) => Int -> a
indexI = (!!) rangeF

indexI2 :: (Ix a, Bounded a, Ix b, Bounded b) => (Int, Int) -> (a, b)
indexI2 = indexI *** indexI

indexI3 :: (Ix a, Bounded a, Ix b, Bounded b, Ix c, Bounded c) => (Int, Int, Int) -> (a, b, c)
indexI3 (x, y, z) = (indexI x, indexI y, indexI z)

ixmapI :: (IArray a e, Ix b, Bounded b) => a Int e -> a b e
ixmapI = ixmap boundsF indexF

ixmapI2 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c) => a (Int, Int) e -> a (b, c) e
ixmapI2 = ixmap boundsF indexF2

ixmapI3 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c, Ix d, Bounded d) => a (Int, Int, Int) e -> a (b, c, d) e
ixmapI3 = ixmap boundsF indexF3

ixmapF :: (IArray a e, Ix b, Bounded b) => a b e -> a Int e
ixmapF a = ixmap (indexF2 $ bounds a) indexI a

ixmapF2 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c) => a (b, c) e -> a (Int, Int) e
ixmapF2 a = ixmap (indexF2x2 $ bounds a) indexI2 a

ixmapF3 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c, Ix d, Bounded d) => a (b, c, d) e -> a (Int, Int, Int) e
ixmapF3 a = ixmap (indexF3x3 $ bounds a) indexI3 a

functionF :: (IArray a e, Ix b, Bounded b) => (b -> e) -> a b e
functionF f = array boundsF $ map (liftM2 (,) id f) rangeF

functionF2 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c) => (b -> c -> e) -> a (b, c) e
functionF2 = functionF . uncurry

functionF3 :: (IArray a e, Ix b, Bounded b, Ix c, Bounded c, Ix d, Bounded d) => (b -> c -> d -> e) -> a (b, c, d) e
functionF3 = functionF . uncurry3
    where uncurry3 f (x, y, z) = f x y z

listI :: (Ix a, Bounded a) => [b] -> a -> b
listI l y = l !! indexF y

listsI :: (Ix a, Bounded a, Ix b, Bounded b) => [[c]] -> a -> b -> c
listsI ls = listI . listI ls

vectorI :: (Ix a, Bounded a, Storable b) => Vector b -> a -> b
vectorI = listI . Lin.toList

matrixI :: (Ix a, Bounded a, Ix b, Bounded b, Element c) => Matrix c -> a -> b -> c
matrixI = listsI . Lin.toLists

finiteFunctions :: forall a b. (Ix a, Bounded a, Ix b, Bounded b) => [a -> b]
finiteFunctions = map listI outputsList
    where inputSize = length (rangeF :: [a])
          outputList = rangeF
          outputsList = sequence . take inputSize $ repeat outputList

instance (Ix a, Bounded a, Eq b) => Eq (a -> b)
    where f == g = all (\a -> f a == g a) rangeF

cartesianProduct2 :: [x] -> [y] -> [(x, y)]
cartesianProduct2 xs ys = (,) <$> xs <*> ys
