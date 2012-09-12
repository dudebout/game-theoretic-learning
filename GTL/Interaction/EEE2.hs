module GTL.Interaction.EEE2 where

import GTL.Interaction.Consistency2 (mockups2)
import GTL.Interaction.Optimality (optimalResponse)
import GTL.Data.Mockup (DeterministicMockupStrategy, Role)
import GTL.Data.Strategy (deterministicStrategiesXZA, randomizeStrategyXZA)
import Data.Ix (Ix)
import GTL.Data.History (History)
import GTL.Data.Signaling (SignalingWXAS2)
import GTL.Data.Finite (cartesianProduct2)
import Numeric.Probability.Distribution (certainly)

isAPureEEE2 :: ( Bounded w, Ix w, Bounded x1, Ix x1, Bounded x2, Ix x2
               , Bounded a1, Ix a1, Bounded s1, Ix s1
               , Bounded a2, Ix a2, Bounded s2, Ix s2
               , Bounded (h1 a1), Ix (h1 a1), Bounded (h1' s1), Ix (h1' s1)
               , Bounded (h2 a2), Ix (h2 a2), Bounded (h2' s2), Ix (h2' s2)
               , History h1, History h1', History h2, History h2'
               ) => Role x1 a1 s1 -> Role x2 a2 s2 -> SignalingWXAS2 w x1 a1 s1 x2 a2 s2
            -> DeterministicMockupStrategy x1 a1 s1 h1 h1' -> DeterministicMockupStrategy x2 a2 s2 h2 h2'
            -> Bool
isAPureEEE2 rol1 rol2 sig strat1 strat2 = opt1 == strat1 && opt2 == strat2
    where (mock1, mock2) = mockups2 rol1 rol2 sig strat1' strat2'
          opt1 = optimalResponse rol1 mock1
          opt2 = optimalResponse rol2 mock2
          strat1' = randomizeStrategyXZA strat1
          strat2' = randomizeStrategyXZA strat2

findPureEEEs2 :: ( Bounded w, Ix w, Bounded x1, Ix x1, Bounded x2, Ix x2
                 , Bounded a1, Ix a1, Bounded s1, Ix s1
                 , Bounded a2, Ix a2, Bounded s2, Ix s2
                 , Bounded (h1 a1), Ix (h1 a1), Bounded (h1' s1), Ix (h1' s1)
                 , Bounded (h2 a2), Ix (h2 a2), Bounded (h2' s2), Ix (h2' s2)
                 , History h1, History h1', History h2, History h2'
                 ) => Role x1 a1 s1 -> Role x2 a2 s2 -> SignalingWXAS2 w x1 a1 s1 x2 a2 s2 -> [(DeterministicMockupStrategy x1 a1 s1 h1 h1', DeterministicMockupStrategy x2 a2 s2 h2 h2')]
findPureEEEs2 rol1 rol2 sig = filter (uncurry $ isAPureEEE2 rol1 rol2 sig) deterministicStrategiesXZA2
    where deterministicStrategiesXZA2 = cartesianProduct2 deterministicStrategiesXZA deterministicStrategiesXZA
