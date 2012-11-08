{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GTL.Learning.OneShot
    ( Role (..), CreateRole (..), Learner (..)
    , Environment (..), Global (..), makeGlob, OneStep (..)
    , Telling, tellActions, tellReports, tellActionsAndReports
    , play, playR
    ) where

import Control.Monad.RWS
import Data.HList (Apply, apply, HMap, hMap)
import GTL.Data.Utility (UtilityAS)
import GTL.Data.HList.HZip3 (HZip3, hUnzip3)
import GTL.Data.HList.HZip4 (HZip4, hZip4)
import GTL.Data.HList.Vector (VHList, toVector)

data Role a s = Role { utility :: UtilityAS a s }

data Learner x a s w = Learner { oneStep :: Role a s -> x -> s -> (a, x, w) }

data Environment rh lh ah sh = E { roles    :: rh
                                 , learners :: lh
                                 , nature   :: ah -> sh }

data Global xh sh ah wh = G { states  :: xh
                            , signals :: sh
                            , actions :: ah
                            , reports :: wh }

makeGlob :: xh -> sh -> Global xh sh ah wh
makeGlob statesH signalsH = G statesH signalsH undefined undefined

data CreateRole = CreateRole
instance Apply CreateRole (UtilityAS a s) (Role a s) where apply _ =  Role

data OneStep = OneStep
instance Apply OneStep (Role a s, Learner x a s w, x, s) (a, x, w) where
    apply _ (r, l, x, s) =  (oneStep l) r x s


oneFullStep :: ( HZip4 r l x s rlxs, HMap OneStep rlxs axw
               , HZip3 a x w axw, Monoid v ) => RWS (Environment r l a s) v (Global x s a w) ()
oneFullStep = do
  G { states  = xh
    , signals = sh } <- get
  E { roles    = rh
    , learners = lh
    , nature   = nat } <- ask
  let axwh'            = hMap OneStep $ hZip4 rh lh xh sh
      (ah', xh', wh') = hUnzip3 axwh'
      sh'             = nat ah'
  put $ G xh' sh' ah' wh'

type Telling v r l x s a w = RWS (Environment r l a s) v (Global x s a w) ()

tellActions :: RWS r [a] (Global x s a w) ()
tellActions = gets actions >>= tell . (:[])

tellReports :: (Monoid v, VHList w v) => RWS r v (Global x s a w) ()
tellReports = gets reports >>= tell . toVector

tellActionsAndReports :: (Monoid v, VHList w v) => RWS r ([a], v) (Global x s a w) ()
tellActionsAndReports = do
  rep <- gets reports
  act <- gets actions
  tell ([act], toVector rep)

playR :: (Monoid v, HMap OneStep rlxs rez, VHList w v, HZip4 r l x s rlxs, HZip3 a x w rez) => Environment r l a s -> Global x s a w -> Int -> ([a], v)
playR env global n = play env global n tellActionsAndReports

play :: (Monoid v, HMap OneStep rlxs rez, HZip4 r l x s rlxs, HZip3 a x w rez) => Environment r l a s -> Global x s a w -> Int -> Telling v r l x s a w -> v
play env global n telling = snd $ evalRWS (replicateM_ n (oneFullStep >> telling)) env global
