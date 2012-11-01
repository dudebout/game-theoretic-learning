{-# LANGUAGE TypeOperators #-}

module GTL.Game.TwoPlayer.Shapley ( module GTL.Game.TwoPlayer
                                  , Action1(..), Signal1, u1
                                  , Action2(..), Signal2, u2
                                  , Actions, Signals
                                  ) where

import GTL.Game.TwoPlayer
import GTL.Data.Utility (UtilityAS)
import Data.Ix (Ix)
import Data.HList (HNil, (:*:))

data Action1 = U | M | D deriving (Show, Bounded, Ix, Eq, Ord)
type Signal1 = Action2

data Action2 = L | C | R deriving (Show, Bounded, Ix, Eq, Ord)
type Signal2 = Action1

u1 :: UtilityAS Action1 Signal1
u1 D L = 1
u1 U C = 1
u1 M R = 1
u1 _ _ = 0

u2 :: UtilityAS Action2 Signal2
u2 R U = 1
u2 L M = 1
u2 C D = 1
u2 _ _ = 0

type Actions = Action1 :*: Action2 :*: HNil
type Signals = Signal1 :*: Signal2 :*: HNil
