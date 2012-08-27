module GTL.Data.MarkovDecisionProcess where

import GTL.Data.Dynamic (DynamicXA)
import GTL.Data.Utility (UtilityXA, Discount)

data MDP x a = MDP { dynamic  :: DynamicXA x a
                   , utility  :: UtilityXA x a
                   , discount :: Discount }
