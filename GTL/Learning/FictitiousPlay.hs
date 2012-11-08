{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GTL.Learning.FictitiousPlay
    ( fictitiousPlay, FPResult(..)
    , empDistOfPlay, margEmpDistOfPlay, ComputeEmpiricalFrequencies(..)
    ) where

import Data.HList (Apply, apply, HMap, hMap)
import Data.Monoid (Monoid)
import GTL.Data.HList.HZip3 (HZip3)
import GTL.Data.HList.HZip4 (HZip4)
import GTL.Data.HList.Vector (VHList, fromVector)
import GTL.Data.HList.List (transposeL, WrapInList(..))
import Data.Ix (Ix)
import Data.Tuple.HList (HLst, toHList, fromHList)
import Numeric.Probability.Distribution (certainly)
import GTL.Data.Time (Time)
import GTL.Data.Utility (UtilityAS)
import GTL.Numeric.Probability (Dist)
import GTL.Learning.OneShot (CreateRole(..), OneStep, Environment( E ), Global, makeGlob, playR)
import GTL.Learning.FictitiousPlay.Learner
import GTL.Numeric.Probability.EmpiricalFrequency (computeEmpiricalFrequencies)

data FPResult a dsh = FPResult { fpActions  :: [a]
                               , fpDistribs :: dsh }

fictitiousPlay :: ( HLst ut uh, HMap CreateRole uh rh, HMap CreateFPLearnerR uh lh, HLst st sh
                  , HMap InitialFPState sh xh, HLst xt xh, HMap OneStep rlxs rez, VHList wh v
                  , Monoid v, HMap MapDistrib wh dsh, HZip4 rh lh xh sh rlxs, HZip3 ah xh wh rez
                  ) => ut -> (ah -> sh) -> st -> Time -> FPResult ah dsh
fictitiousPlay ut nat st n = FPResult ahs dst
    where uh         = toHList ut
          env        = E (hMap CreateRole uh) (hMap CreateFPLearnerR uh) nat
          initGlobal = makeFPGlob st
          (ahs, v)   = playR env initGlobal n
          dst        = hMap MapDistrib $ fromVector v

data InitialFPState = InitialFPState
instance Apply InitialFPState s (FPState s)
    where apply _ s =  initialFPState $ certainly s

makeFPGlob :: (HLst st sh, HMap InitialFPState sh xh, HLst xt xh) => st -> Global xh sh ah wh
makeFPGlob signalsT = makeGlob statesH signalsH
    where signalsH = toHList signalsT
          statesH  = hMap InitialFPState signalsH

data CreateFPLearnerR = CreateFPLearnerR
instance ( Bounded a, Ix a
         , Bounded s, Ix s ) => Apply CreateFPLearnerR (UtilityAS a s) (FPLearnerR a s)
    where apply _ _ = classicFPLearnerR

data MapDistrib = MapDistrib
instance Apply MapDistrib [FPState s] [Dist s]
    where apply _ x = map distrib x

empDistOfPlay :: (Ord at, HLst at ah) => FPResult ah dst -> [Dist at]
empDistOfPlay = computeEmpiricalFrequencies . (map fromHList) . fpActions

margEmpDistOfPlay :: ( HMap WrapInList ah bh, Monoid v, VHList bh v
                     , HMap ComputeEmpiricalFrequencies bh ch
                     ) => FPResult ah dst -> ch
margEmpDistOfPlay = (hMap ComputeEmpiricalFrequencies) . transposeL . fpActions

data ComputeEmpiricalFrequencies = ComputeEmpiricalFrequencies
instance (Ord a) => Apply ComputeEmpiricalFrequencies [a] [Dist a]
    where apply _  = computeEmpiricalFrequencies
