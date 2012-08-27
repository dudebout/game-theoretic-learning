module GTL.Interaction.Optimality2 (optimalResponse2) where

import GTL.Data.Mockup (Mockup, toMockupMDP, fromMockupMDP, DeterministicMockupStrategy, Role)
import GTL.Numeric.DynamicProgramming (optimalStrategy)

import Data.Ix (Ix)
import GTL.Data.History (History)

optimalResponse2 :: ( Bounded x1, Ix x1, Bounded x2, Ix x2
                    , Bounded a1, Ix a1, Bounded s1, Ix s1
                    , Bounded a2, Ix a2, Bounded s2, Ix s2
                    , Bounded (h1 a1), Ix (h1 a1), Bounded (h1' s1), Ix (h1' s1)
                    , Bounded (h2 a2), Ix (h2 a2), Bounded (h2' s2), Ix (h2' s2)
                    , History h1, History h1', History h2, History h2'
                    ) => Role x1 a1 s1 -> Role x2 a2 s2 -> Mockup a1 s1 h1 h1' -> Mockup a2 s2 h2 h2'
                 -> (DeterministicMockupStrategy x1 a1 s1 h1 h1', DeterministicMockupStrategy x2 a2 s2 h2 h2')
optimalResponse2 rol1 rol2 mock1 mock2 = (opt1, opt2)
    where opt1 = fromMockupMDP $ optimalStrategy $ toMockupMDP rol1 mock1
          opt2 = fromMockupMDP $ optimalStrategy $ toMockupMDP rol2 mock2
