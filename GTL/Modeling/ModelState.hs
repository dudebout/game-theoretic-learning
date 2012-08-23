{-# LANGUAGE FlexibleContexts #-}

module GTL.Modeling.ModelState where


import Text.PrettyPrint.Leijen (Pretty, pretty, tupled)
import GTL.Data.Finite (Ix)
import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (certainly)
import GTL.Data.History (History(..))
import GTL.Data.History.Instances (History0)

data ModelState a s h h' = ModelState { actions :: (h a)
                                      , signals :: (h' s) } deriving (Show, Bounded, Ix, Ord, Eq)
instance (Pretty (h a), Pretty (h' s)) => Pretty (ModelState a s h h') where
    pretty (ModelState ha hs) = tupled [pretty ha, pretty hs]

addToModelState :: (History h, History h') =>
                   ModelState a s h h' -> a -> s -> ModelState a s h h'
addToModelState (ModelState ha hs) a s = ModelState (addToHistory ha a) (addToHistory hs s)

type Observation y z = y -> Dist z
type DirectObservation y = Observation y y

perfectlyObserve :: DirectObservation y
perfectlyObserve = certainly

type ExogenousModelState a s h = ModelState a s History0 h

exogenize :: ExogenousModelState a s h -> h s
exogenize = signals

exogenousPerfectObservation :: Observation (ExogenousModelState a s h) (h s)
exogenousPerfectObservation = perfectlyObserve . exogenize
