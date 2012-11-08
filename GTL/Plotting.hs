{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GTL.Plotting where

import Graphics.Rendering.Plot.Figure
import GTL.Data.Finite (rangeF)
import GTL.Numeric.Probability (Dist, Proba, (?!))
import Data.Ix (Ix)
import Data.HList (Apply, apply)
import Numeric.LinearAlgebra (Vector, fromList, linspace, dim)

probabilityVsTime :: Ord a => [Dist a] -> a -> (Vector Proba)
probabilityVsTime d x = fromList $ map (?! x) d

distributionVsTime :: (Ord a, Bounded a, Ix a) => [Dist a] -> [(Vector Proba)]
distributionVsTime d = map (probabilityVsTime d) rangeF

labelledProbabilityVsTime :: (Ord a, Show a) => [Dist a] -> a -> (Series, SeriesLabel)
labelledProbabilityVsTime d x = (probabilityVsTime d x, show x)

labelledDistributionVsTime :: (Ord a, Show a, Ix a, Bounded a) => [Dist a] -> [(Series, SeriesLabel)]
labelledDistributionVsTime d = map (labelledProbabilityVsTime d) rangeF

timeSeries :: [(Series, SeriesLabel)] -> (SeriesType, Series, [(Series, SeriesLabel)])
timeSeries as = (Line, times, as)
    where n = dim $ fst $ head as
          times = linspace n (1, fromIntegral n)

figure :: Plot () -> Figure ()
figure p = setPlots 1 1 >> withPlot (1, 1) p

subfigures :: [Plot ()] -> Figure ()
subfigures ps = do setPlots num 1
                   mapM_ (\(p, n) -> withPlot (n, 1) p) $ zip ps [1..]
    where num = length ps

linlinP :: Dataset d => d -> Plot ()
linlinP ds = do setDataset ds
                addAxis XAxis (Side Lower) $ return ()
                addAxis YAxis (Side Lower) $ return ()
                setRangeFromData XAxis Lower Linear
                setRangeFromData YAxis Lower Linear

linlogP :: Dataset d => d -> Plot ()
linlogP ds = do setDataset ds
                addAxis XAxis (Side Lower) $ return ()
                addAxis YAxis (Side Lower) $ return ()
                setRangeFromData XAxis Lower Linear
                setRangeFromData YAxis Lower Log

loglinP :: Dataset d => d -> Plot ()
loglinP ds = do setDataset ds
                addAxis XAxis (Side Lower) $ return ()
                addAxis YAxis (Side Lower) $ return ()
                setRangeFromData XAxis Lower Log
                setRangeFromData YAxis Lower Linear

loglogP :: Dataset d => d -> Plot ()
loglogP ds = do setDataset ds
                addAxis XAxis (Side Lower) $ return ()
                addAxis YAxis (Side Lower) $ return ()
                setRangeFromData XAxis Lower Log
                setRangeFromData YAxis Lower Log

legendsOn :: Figure ()
legendsOn = withPlots legendOn

legendOn :: Plot ()
legendOn = setLegend True NorthEast Inside

distVsTime :: (Bounded a, Ix a, Show a) => [Dist a] -> (SeriesType, Series, [(Series, SeriesLabel)])
distVsTime d = timeSeries $ labelledDistributionVsTime d

data DistVsTime = DistVsTime
instance (Bounded a, Ix a, Show a) => Apply DistVsTime [Dist a] (SeriesType, Series, [(Series, SeriesLabel)])
    where apply _ = distVsTime
