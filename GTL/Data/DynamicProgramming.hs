{-# LANGUAGE ScopedTypeVariables #-}

module GTL.Data.DynamicProgramming where

import GTL.Data.Utility (UtilityXA, Value, Discount)
import GTL.Data.Dynamic (DynamicXA)
import GTL.Data.MarkovDecisionProcess (MDP(..))
import Data.Ix (Ix)
import GTL.Data.Finite (functionF2, functionF3, ixmapF2, ixmapF3, rangeF)
import GTL.Numeric.Probability (Proba, (?!))
import Numeric.LinearAlgebra (Matrix, Vector)
import Data.Array.Unboxed (UArray)

type StateI = Int
type ActionI = Int

type DynamicsI = UArray (ActionI, StateI, StateI) Proba
type UtilityI = UArray (StateI, ActionI) Value

type DeterministicStrategyV = Vector ActionI
type StateValueV = Vector Value
type ActionValueV = Vector Value
type StateTransitionV = Matrix Proba

type QFactorIV = StateI -> ActionValueV

data Data = Data { stateCard  :: StateI
                 , actionCard :: ActionI
                 , dynamicsI  :: DynamicsI
                 , utilityI   :: UtilityI
                 , discountD  :: Discount
                 } deriving (Show)

data Result = Result { valueFunction :: StateValueV
                     , qFactor       :: QFactorIV }

setup :: forall x a. (Bounded x, Ix x, Bounded a, Ix a) => MDP x a -> Data
setup (MDP dyn util disc) = Data { stateCard  = xCard
                                 , actionCard = aCard
                                 , dynamicsI  = toDynamicsI dyn
                                 , utilityI   = toUtilityI util
                                 , discountD  = disc }
    where xCard = length (rangeF :: [x])
          aCard = length (rangeF :: [a])

toUtilityI :: (Ix x, Bounded x, Ix a, Bounded a) => UtilityXA x a -> UtilityI
toUtilityI = ixmapF2 . functionF2

toDynamicsI :: (Ix x, Bounded x, Ix a, Bounded a) => DynamicXA x a -> DynamicsI
toDynamicsI dyn = ixmapF3 . functionF3 $ dyn'
    where dyn' a x x' =  dyn x a ?! x'
