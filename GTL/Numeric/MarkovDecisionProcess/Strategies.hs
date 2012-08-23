module GTL.Numeric.MarkovDecisionProcess.Strategies (gibbsStrategy, valFunc, optimalStrategy, resolve, createProblem, Problem(..), qFac) where

import Data.Array.IArray
import GTL.Data.Finite
import GTL.Numeric.Probability
import GTL.Numeric.Probability.Finite hiding (toList)
import GTL.Numeric.MarkovDecisionProcess.Types
import GTL.Numeric.MarkovDecisionProcess.PolicyIteration (Data (..), Result (..), solve, Vars (..))
import Numeric.LinearAlgebra hiding (fromList)
import Control.Monad (liftM2)

toUtilityI :: (Ix x, Bounded x, Ix a, Bounded a) => Utility x a -> UtilityI
toUtilityI = ixmapF2 . functionF2

toDynamicsI :: (Ix x, Bounded x, Ix a, Bounded a) => Dynamics x a -> DynamicsI
toDynamicsI dyn = ixmapF3 . functionF3 $ dyn'
    where dyn' a x x' =  dyn x a ?! x'

data Problem x a = Problem { stateDomain :: [x]
                           , actionDomain :: [a]
                           , mdp :: Data }

createProblem :: (Ix x, Bounded x,
                  Ix a, Bounded a) => Dynamics x a -> Utility x a -> Discount -> Problem x a
createProblem dyn util disc = Problem { stateDomain = xDomain
                                      , actionDomain = aDomain
                                      , mdp = Data { stateCard = xCard
                                                   , actionCard = aCard
                                                   , dynamicsI = toDynamicsI dyn
                                                   , utilityI = toUtilityI util
                                                   , discount = disc } }
    where xDomain = rangeF
          aDomain = rangeF
          xCard = length xDomain
          aCard = length aDomain

valF :: (Ix x, Bounded x) => Problem x a -> StateValueV -> x -> Value
valF _ = vectorI

stratF :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> DeterministicStrategyV -> x -> a
stratF _ strat = indexI . vectorI strat

varsF :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> Vars -> (x -> Value, x -> a)
varsF prob = liftM2 (,) (valF prob . value) (stratF prob . strategy)

-- prob :: (Ix x, Bounded x) => Problem x a -> StateValueV -> (x -> Value)
-- probValue _ = vectorI

-- pretty []  => Pretty (Vars) where
--     pretty fun = vcat [hsep [pretty a, arrow, align $ pretty $ fun a] | a <- rangeF]


toStrategyF :: (Ix x, Bounded x, Ix a, Bounded a) => DeterministicStrategyV -> DeterministicStrategyF x a
toStrategyF v = indexI . vectorI v


-- optimalStrategy :: (Ix x, Bounded x, Ix a, Bounded a) => Dynamics x a -> Utility x a -> Discount -> (x -> a)
-- optimalStrategy dyn pay disc x = indexI $ maxIndex $ qF $ indexF x
--     where problem = createProblem dyn pay disc
--           Result { qFactor = qF } = solve $ mdp problem



gibbsStrategy :: (Ix x, Bounded x, Ix a, Bounded a) => Dynamics x a -> Utility x a -> Discount -> Epsilon -> x -> Dist a
gibbsStrategy dyn pay disc epsi x = fromList $ toList $ mapVector (\y -> exp $ y / epsi) $ qF $ indexF x
    where problem = createProblem dyn pay disc
          Result { qFactor = qF } = solve $ mdp problem
-- solveP :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> [DeterministicStrategyF x a]
-- solveP p = map (toStrategyF . strategy) $ (\(_, _, x) -> x) $ solve $ mdp p

resolve :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> Result
resolve = solve . mdp

valFunc :: (Ix x, Bounded x) => Problem x a -> Result -> x -> Value
valFunc _ (Result v _) x = v @> indexF x

qFac :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> Result -> (x, a) -> Value
qFac _ (Result _ qF) (x, a) = qF (indexF x) @> indexF a

optimalStrategy :: (Ix x, Bounded x, Ix a, Bounded a) => Problem x a -> Result -> x -> a
optimalStrategy _ (Result _ qF) = indexI . maxIndex . qF . indexF


type QFactorIV = StateI -> Vector Value
