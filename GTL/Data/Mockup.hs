module GTL.Data.Mockup ( MockupState(..), addToMockupState
                       , ExogenousMockupState, exogenize
                       , Mockup, MockupStrategy, DeterministicMockupStrategy
                       , Role(..), MockupMDP, toMockupMDP, fromMockupMDP ) where

import Data.Ix (Ix(..))
import GTL.Numeric.Probability (Dist)
import GTL.Data.History (History, addToHistory)
import GTL.Data.History.Instances (History0)
import GTL.Data.Dynamic (DynamicXAS)
import GTL.Data.Utility (UtilityXAS, Discount)
import GTL.Data.MarkovDecisionProcess (MDP(..))
import GTL.Data.Strategy (StrategyXYA, DeterministicStrategyXYA)
import Numeric.Probability.Distribution (expected)

data MockupState a s h h' = MockupState { actions :: h a
                                        , signals :: h' s } deriving (Bounded, Eq, Ix, Ord, Show)

addToMockupState :: (History h, History h') =>
                    MockupState a s h h' -> a -> s -> MockupState a s h h'
addToMockupState (MockupState ha hs) a s = MockupState (addToHistory ha a) (addToHistory hs s)

type ExogenousMockupState a s h = MockupState a s History0 h

exogenize :: ExogenousMockupState a s h -> h s
exogenize = signals

type Mockup a s h h' = MockupState a s h h' -> Dist s

type MockupStrategy x a s h h' = StrategyXYA x (MockupState a s h h') a
type DeterministicMockupStrategy x a s h h' = DeterministicStrategyXYA x (MockupState a s h h') a

data Role x a s = Role { augmentedDynamic  :: DynamicXAS x a s
                       , augmentedUtility  :: UtilityXAS x a s
                       , augmentedDiscount :: Discount }

type MockupMDP x a s h h' = MDP (x, MockupState a s h h') a

toMockupMDP :: (History h, History h') => Role x a s -> Mockup a s h h' -> MockupMDP x a s h h'
toMockupMDP (Role dyn util disc) mock = MDP mockDyn mockUtil disc
    where mockDyn (x, y) a = do
            s <- mock y
            x' <- dyn x a s
            let y' = addToMockupState y a s
            return (x', y')
          mockUtil (x, y) a = expected $ fmap (util x a) $ mock y

fromMockupMDP :: ((x, y) -> a) -> x -> y -> a
fromMockupMDP = curry