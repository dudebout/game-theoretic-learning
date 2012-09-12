module GTL.Data.Mockup ( MockupState(..), updateMockupState
                       , ExogenousMockupState, exogenize
                       , Mockup, updateMockup
                       , MockupArray, mockupToArray, arrayToMockup
                       , MockupStrategy, DeterministicMockupStrategy
                       , Role(..), MockupMDP, toMockupMDP, fromMockupMDP ) where

import Data.Ix (Ix(..))
import GTL.Numeric.Probability (Dist, Proba, (?!))
import GTL.Numeric.Probability.Finite (fromList)
import GTL.Data.History (History, addToHistory)
import GTL.Data.History.Instances (History0)
import GTL.Data.Dynamic (DynamicXAS)
import GTL.Data.Utility (UtilityXAS, Discount)
import GTL.Data.Finite (functionF2, boundsF)
import GTL.Data.MarkovDecisionProcess (MDP(..))
import GTL.Data.Strategy (StrategyXZA, DeterministicStrategyXZA)
import Numeric.Probability.Distribution (expected, unfold, relative)
import Data.Array (Array, elems, ixmap)

type Friction = Double

data MockupState a s h h' = MockupState { actions :: h a
                                        , signals :: h' s } deriving (Bounded, Eq, Ix, Ord, Show)

updateMockupState :: (History h, History h') =>
                     MockupState a s h h' -> a -> s -> MockupState a s h h'
updateMockupState (MockupState ha hs) a s = MockupState (addToHistory ha a) (addToHistory hs s)

type ExogenousMockupState a s h = MockupState a s History0 h

exogenize :: ExogenousMockupState a s h -> h s
exogenize = signals

type Mockup a s h h' = MockupState a s h h' -> Dist s

updateMockup :: (Bounded s, Ix s, Ord s) => Friction -> Mockup a s h h' -> Mockup a s h h' -> Mockup a s h h'
updateMockup step mock mock' y = unfold $ relative [1 - step, step] [mock y, mock' y]

type MockupArray a s h h' = Array (MockupState a s h h', s) Proba

mockupToArray :: (Bounded z, Ix z, Bounded s, Ix s) => (z -> Dist s) -> Array (z, s) Proba
mockupToArray mock = functionF2 $ \z s -> mock z ?! s

arrayToMockup :: (Bounded z, Ix z, Bounded s, Ix s) => Array (z, s) Proba -> z -> Dist s
arrayToMockup arr z = fromList $ elems $ ixmap boundsF (\s -> (z, s)) arr

type MockupStrategy x a s h h' = StrategyXZA x (MockupState a s h h') a
type DeterministicMockupStrategy x a s h h' = DeterministicStrategyXZA x (MockupState a s h h') a

data Role x a s = Role { augmentedDynamic  :: DynamicXAS x a s
                       , augmentedUtility  :: UtilityXAS x a s
                       , augmentedDiscount :: Discount }

type MockupMDP x a s h h' = MDP (x, MockupState a s h h') a

toMockupMDP :: (History h, History h') => Role x a s -> Mockup a s h h' -> MockupMDP x a s h h'
toMockupMDP (Role dyn util disc) mock = MDP mockDyn mockUtil disc
    where mockDyn (x, z) a = do
            s <- mock z
            x' <- dyn x a s
            let z' = updateMockupState z a s
            return (x', z')
          mockUtil (x, z) a = expected $ fmap (util x a) $ mock z

fromMockupMDP :: ((x, z) -> a) -> x -> z -> a
fromMockupMDP = curry
