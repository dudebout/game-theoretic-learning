{-# LANGUAGE TypeOperators #-}

module GTL.Game.ThreePlayer.Jordan ( module GTL.Game.ThreePlayer
                                   , Action1(..), Signal1, u1
                                   , Action2(..), Signal2, u2
                                   , Action3(..), Signal3, u3
                                   , Actions, Signals
                                   ) where

import GTL.Game.ThreePlayer
import GTL.Data.Utility (UtilityAS)
import Data.Ix (Ix)
import Data.HList (HNil, (:*:))

data Action1 = H1 | T1 deriving (Show, Bounded, Ix, Eq, Ord)
data Action2 = H2 | T2 deriving (Show, Bounded, Ix, Eq, Ord)
data Action3 = H3 | T3 deriving (Show, Bounded, Ix, Eq, Ord)

type Signal1 = (Action2, Action3)
type Signal2 = (Action1, Action3)
type Signal3 = (Action1, Action2)

u1 :: UtilityAS Action1 Signal1
u1 H1 (H2, _) = 1
u1 T1 (T2, _) = 1
u1 _  _       = -1

u2 :: UtilityAS Action2 Signal2
u2 H2 (_, H3) = 1
u2 T2 (_, T3) = 1
u2 _  _       = -1

u3 :: UtilityAS Action3 Signal3
u3 H3 (T1, _) = 1
u3 T3 (H1, _) = 1
u3 _  _       = -1

type Actions = Action1 :*: Action2 :*: Action3 :*: HNil
type Signals = Signal1 :*: Signal2 :*: Signal3 :*: HNil
