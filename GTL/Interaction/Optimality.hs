module GTL.Interaction.Optimality ( optimalResponse
                                  , uniformSmoothedOptimalResponse
                                  , gibbsSmoothedOptimalResponse ) where

import GTL.Data.Mockup (Mockup, toMockupMDP, fromMockupMDP, MockupStrategy, DeterministicMockupStrategy, Role)
import GTL.Numeric.DynamicProgramming (optimalStrategy, uniformSmoothedOptimalStrategy, gibbsSmoothedOptimalStrategy)
import GTL.Data.Utility (Epsilon)

import Data.Ix (Ix)
import GTL.Data.History (History)

optimalResponse :: ( Bounded x, Ix x, Bounded a, Ix a, Bounded s, Ix s
                   , Bounded (h a), Ix (h a), Bounded (h' s), Ix (h' s)
                   , History h, History h'
                   ) => Role x a s -> Mockup a s h h'
                 -> DeterministicMockupStrategy x a s h h'
optimalResponse rol mock = fromMockupMDP $ optimalStrategy $ toMockupMDP rol mock

uniformSmoothedOptimalResponse :: ( Bounded x, Ix x, Bounded a, Ix a, Bounded s, Ix s
                                  , Bounded (h a), Ix (h a), Bounded (h' s), Ix (h' s)
                                  , History h, History h'
                                  ) => Role x a s -> Epsilon -> Mockup a s h h'
                               -> MockupStrategy x a s h h'
uniformSmoothedOptimalResponse rol eps mock = fromMockupMDP $ uniformSmoothedOptimalStrategy (toMockupMDP rol mock) eps

gibbsSmoothedOptimalResponse :: ( Bounded x, Ix x, Bounded a, Ix a, Bounded s, Ix s
                                , Bounded (h a), Ix (h a), Bounded (h' s), Ix (h' s)
                                , History h, History h'
                                ) => Role x a s -> Epsilon -> Mockup a s h h'
                             -> MockupStrategy x a s h h'
gibbsSmoothedOptimalResponse rol eps mock = fromMockupMDP $ gibbsSmoothedOptimalStrategy (toMockupMDP rol mock) eps
