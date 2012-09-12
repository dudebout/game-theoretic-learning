module GTL.Numeric.DynamicProgramming ( optimalStrategy
                                      , uniformSmoothedOptimalStrategy
                                      , gibbsSmoothedOptimalStrategy ) where

import GTL.Data.MarkovDecisionProcess (MDP)
import GTL.Data.Strategy (DeterministicStrategyXA, StrategyXA)
import Data.Ix (Ix)
import GTL.Data.Finite (indexI, indexF)
import GTL.Numeric.Probability.Finite hiding (toList)
import GTL.Data.DynamicProgramming (setup, Result(..))
import GTL.Numeric.DynamicProgramming.PolicyIteration (solve)
import GTL.Data.Unitless (Unitless)
import GTL.Data.Utility (Epsilon)
import Numeric.LinearAlgebra (maxIndex, mapVector, toList)

optimalStrategy :: (Bounded x, Ix x, Bounded a, Ix a) => MDP x a -> DeterministicStrategyXA x a
optimalStrategy mdp = indexI . maxIndex . qF . indexF
    where (Result _ qF) = solve $ setup mdp

uniformSmoothedOptimalStrategy :: (Bounded x, Ix x, Bounded a, Ix a) => MDP x a -> Unitless -> StrategyXA x a
uniformSmoothedOptimalStrategy mdp epsi x = almostCertainly epsi (optimalStrategy mdp x)

gibbsSmoothedOptimalStrategy :: (Bounded x, Ix x, Bounded a, Ix a) => MDP x a -> Epsilon -> StrategyXA x a
gibbsSmoothedOptimalStrategy mdp epsi x = fromList $ toList $ mapVector (\y -> exp $ - y / epsi) $ qF $ indexF x
    where (Result _ qF) = solve $ setup mdp
