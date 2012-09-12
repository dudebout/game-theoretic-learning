module GTL.Interaction.SimulatedConsistency2 where

import GTL.Data.Mockup
import GTL.Data.History
import GTL.Data.Dynamic
import GTL.Data.Signaling
import System.Random
import Numeric.Probability.Random
import Numeric.Probability.Distribution (uniform)
import qualified Numeric.Probability.Distribution as Dist (map)
import Control.Monad.RWS (RWS, evalRWS, ask, get, put, tell)
import Control.Monad

data Param w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = Param { dynamic1  :: DynamicXAS x1 a1 s1
                                                     , dynamic2  :: DynamicXAS x2 a2 s2
                                                     , strategy1 :: MockupStrategy x1 a1 s1 h1 h1'
                                                     , strategy2 :: MockupStrategy x2 a2 s2 h2 h2'
                                                     , signaling :: SignalingWXAS2 w x1 a1 s1 x2 a2 s2
                                                     }

data Var w x1 z1 x2 z2 = Var { state     :: (w, x1, z1, x2, z2)
                             , generator :: StdGen } deriving (Show)

type Log a1 s1 h1 h1' a2 s2 h2 h2' = ((MockupState a1 s1 h1 h1', s1), (MockupState a2 s2 h2 h2', s2))

type Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' = RWS (Param w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2') [Log a1 s1 h1 h1' a2 s2 h2 h2'] (Var w x1 (MockupState a1 s1 h1 h1') x2 (MockupState a2 s2 h2 h2'))

runComp :: Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' comp -> Param w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' -> Var w x1 (MockupState a1 s1 h1 h1') x2 (MockupState a2 s2 h2 h2') -> [Log a1 s1 h1 h1' a2 s2 h2 h2']
runComp comp param var = snd $ evalRWS comp param var

onePass :: (History h1, History h1', History h2, History h2') => Comp w x1 a1 s1 h1 h1' x2 a2 s2 h2 h2' ()
onePass = do
  Param { dynamic1  = dyn1
        , dynamic2  = dyn2
        , strategy1 = strat1
        , strategy2 = strat2
        , signaling = sig } <- ask
  Var { state     = (w, x1, z1, x2, z2)
      , generator = gen } <- get
  let (w', x1', z1', s1', x2', z2', s2') = runSeed gen $ do
                                             a1'            <- pick $ strat1 x1 z1
                                             a2'            <- pick $ strat2 x2 z2
                                             (w', s1', s2') <- pick $ sig w x1 a1' x2 a2'
                                             x1'            <- pick $ dyn1 x1 a1' s1'
                                             x2'            <- pick $ dyn2 x2 a2' s2'
                                             let z1'         = updateMockupState z1 a1' s1'
                                                 z2'         = updateMockupState z2 a2' s2'
                                             return (w', x1', z1', s1',x2', z2', s2')
  let (_, gen') = next gen
  tell [((z1', s1'), (z2', s2'))]
  put $ Var (w', x1', z1', x2', z2') gen'

simulatedMockups2 rol1 rol2 sig strat1 strat2 gen initState = (mock1, mock2)
    where trace = runComp (replicateM 100 onePass) p v
          p = Param (augmentedDynamic rol1) (augmentedDynamic rol2) strat1 strat2 sig
          v = Var initState gen
          -- THIS IS NOT GENERAL JUST A TEMPORARY HACK FOR DEPTH-0 ONLY
          trace1 = map (snd . fst) trace
          trace2 = map (snd . snd) trace
          -- THIS IS NOT GENERAL JUST A TEMPORARY HACK FOR DEPTH-0 ONLY
          mock1 = const $ uniform trace1
          mock2 = const $ uniform trace2
