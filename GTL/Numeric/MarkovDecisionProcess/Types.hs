module GTL.Numeric.MarkovDecisionProcess.Types where

import GTL.Numeric.Probability (Proba, Dist)
import Data.Array.Unboxed (UArray)
import Numeric.LinearAlgebra (Matrix, Vector)

type Value = Double
type Discount = Double

type Dynamics x a = x -> a -> Dist x
type Utility x a = x -> a -> Value
type Strategy x a = x -> Dist a
type DeterministicStrategy x a = x -> a
type DeterministicStrategyF x a = DeterministicStrategy x a

type Epsilon = Double

type StateI = Int
type ActionI = Int

type DynamicsI = UArray (ActionI, StateI, StateI) Proba
type UtilityI = UArray (StateI, ActionI) Value

type DeterministicStrategyV = Vector ActionI
type StateValueV = Vector Value
type ActionValueV = Vector Value
type StateTransitionV = Matrix Proba

type QFactorIV = StateI -> ActionValueV
