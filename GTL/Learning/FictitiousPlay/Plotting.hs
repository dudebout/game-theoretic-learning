{-# LANGUAGE FlexibleContexts #-}
module GTL.Learning.FictitiousPlay.Plotting where

import GTL.Learning.FictitiousPlay (FPResult(..), empDistOfPlay, margEmpDistOfPlay, ComputeEmpiricalFrequencies)
import Data.Ix (Ix)
import GTL.Plotting (linlinP, distVsTime, DistVsTime(..), legendOn)
import Graphics.Rendering.Plot.Figure (Series, SeriesType, SeriesLabel, Plot, withHeading, setText)
import Data.HList (HMapOut, hMapOut, HMap)
import Data.Monoid (Monoid)
import GTL.Data.HList.List (WrapInList)
import GTL.Data.HList.Vector (VHList)

import Data.Tuple.HList

fpPlots :: ( Bounded a, Show a, Ix a
           , HMapOut DistVsTime r (SeriesType, Series, [(Series, SeriesLabel)])
           , HMapOut DistVsTime dst (SeriesType, Series, [(Series, SeriesLabel)])
           , HMap ComputeEmpiricalFrequencies bh r, HMap WrapInList ah bh
           , Monoid v, VHList bh v, HLst a ah
           ) => FPResult ah dst -> [Plot ()]
fpPlots result = map assemble $ zip plots titles
    where jointPlot  = linlinP $ distVsTime $ empDistOfPlay result
          margPlots  = map linlinP $ makeDataSet $ margEmpDistOfPlay result
          obsPlots   = map linlinP $ makeDataSet $ fpDistribs result
          num        = length margPlots
          jointTitle = "Joint empirical distribution of play"
          margTitles = zipWith (++) (repeat "Marginal distribution of actions for player ") (map show [1..num])
          obsTitles  = zipWith (++) (repeat "Empirical frequencies observed by player ") (map show [1..])
          plots      = jointPlot  : margPlots  ++ obsPlots
          titles     = jointTitle : margTitles ++ obsTitles
          assemble (pl, title) = do pl
                                    legendOn
                                    withHeading $ setText title

-- This function makes sure that the type is fixed before linlinP tries to work
-- on the data.
makeDataSet :: HMapOut DistVsTime r (SeriesType, Series, [(Series, SeriesLabel)]) => r -> [(SeriesType, Series, [(Series, SeriesLabel)])]
makeDataSet = hMapOut DistVsTime
