module GTL.Numeric.DynamicProgramming.PolicyIteration where

import GTL.Data.Utility (Value)
import GTL.Data.DynamicProgramming
import GTL.Numeric.Probability (Proba)
import Numeric.LinearAlgebra
import Data.Array.Unboxed
import Control.Monad.RWS (RWS, evalRWS, get, gets, modify, ask, tell)
import Control.Monad.Loops (untilM_)

data Vars = Vars { value             :: StateValueV
                 , strategy          :: DeterministicStrategyV
                 , strategyUnchanged :: Bool
                 } deriving (Show)

type Debug = [Vars]

type Comp = RWS Data Debug Vars

computeResult :: Comp Result
computeResult = do
  v <- gets value
  qFactor' <- computeUtilWithVal
  return $ Result v qFactor'

solve :: Data -> Result
solve mdp = fst $ eval (policyIteration >> computeResult) mdp

debugSolve :: Data -> Debug
debugSolve mdp = snd $ eval policyIteration mdp

eval :: Comp a -> Data -> (a, Debug)
eval comp mdp = evalRWS comp mdp (initVars mdp)

initVars :: Data -> Vars
initVars mdp                            = Vars { value = undefined
                    , strategy          = strat
                    , strategyUnchanged = False
                    }
    where xCard = stateCard mdp
          strat = buildVector xCard $ const 0

policyIteration :: Comp ()
policyIteration = untilM_ alternateUpdates (gets strategyUnchanged)
    where alternateUpdates = do
            updateStateValue
            updateDeterministicStrategy
            vars <- get
            tell [vars]

computeStateValue :: Comp StateValueV
computeStateValue = do
  Data { stateCard = xCard
       , dynamicsI = dyn
       , utilityI  = util
       , discountD = disc
       } <- ask
  strat <- gets strategy
  let utilWithStrat = buildVector xCard $ util <# strat
      dynWithStrat = buildMatrix xCard xCard $ dyn <## strat
      identity = ident xCard
  return $ (identity - scale disc dynWithStrat) <\> scale (1 - disc) utilWithStrat

-- |Computes a state value vector
updateStateValue :: Comp ()
updateStateValue = do
  val <- computeStateValue
  modify (\s -> s { value = val })

computeDeterministicStrategy :: Comp DeterministicStrategyV
computeDeterministicStrategy = do
  Data { stateCard = xCard } <- ask
  utilWithVal <- computeUtilWithVal
  return $ buildVector xCard (maxIndex . utilWithVal)

-- |Computes a strategy
updateDeterministicStrategy :: Comp ()
updateDeterministicStrategy = do
  strat <- gets strategy
  strat' <- computeDeterministicStrategy
  modify $ \s -> s { strategy = strat'
                   , strategyUnchanged = strat == strat' }

computeUtilWithVal :: Comp QFactorIV
computeUtilWithVal = do
  Data { stateCard  = xCard
       , actionCard = aCard
       , dynamicsI  = dyn
       , utilityI   = util
       , discountD  = disc
       } <- ask
  val <- gets value
  let oneShotUtil x = buildVector aCard $ util <! x
      transition x = buildMatrix aCard xCard $ dyn <!! x
      utilWithVal x = scale (1 - disc) (oneShotUtil x) + scale disc (transition x) <> val
  return utilWithVal

-- |Applies a deterministic strategy to a utility function
(<#) :: UtilityI -> DeterministicStrategyV -> StateI -> Value
(util <# strat) x = util ! (x, strat @> x)

-- |Applies a deterministic strategy to a transition probability
(<##) :: DynamicsI -> DeterministicStrategyV -> (StateI, StateI) -> Proba
(dyn <## strat) (x, x') = dyn ! (strat @> x, x, x')

-- |
(<!) :: UtilityI -> StateI -> ActionI -> Value
(util <! x) a = util ! (x, a)

-- |
(<!!) :: DynamicsI -> StateI -> (StateI, ActionI) -> Proba
(dyn <!! x) (a, x') = dyn ! (a, x, x')
