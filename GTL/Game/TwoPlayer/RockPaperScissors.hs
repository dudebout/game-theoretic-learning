{-# LANGUAGE TypeOperators #-}

module GTL.Game.TwoPlayer.RockPaperScissors ( module GTL.Game.TwoPlayer
                                            , Action1(..), Signal1, u1
                                            , Action2(..), Signal2, u2
                                            , Actions, Signals
                                            ) where

import GTL.Game.TwoPlayer
import GTL.Data.Utility (UtilityAS)
import Data.Ix (Ix)
import Data.HList (HNil, (:*:))

data Action1 = R1 | P1 | S1 deriving (Show, Bounded, Ix, Eq, Ord)
type Signal1 = Action2

data Action2 = R2 | P2 | S2 deriving (Show, Bounded, Ix, Eq, Ord)
type Signal2 = Action1

u1 :: UtilityAS Action1 Signal1
u1 R1 S2 = 1
u1 P1 R2 = 1
u1 S1 P2 = 1
u1 R1 R2 = 0
u1 P1 P2 = 0
u1 S1 S2 = 0
u1 _  _  = -1

u2 :: UtilityAS Action2 Signal2
u2 R2 S1 = 1
u2 P2 R1 = 1
u2 S2 P1 = 1
u2 R2 R1 = 0
u2 P2 P1 = 0
u2 S2 S1 = 0
u2 _  _  = -1

type Actions = Action1 :*: Action2 :*: HNil
type Signals = Signal1 :*: Signal2 :*: HNil
