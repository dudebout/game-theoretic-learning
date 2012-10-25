module GTL.Game.TwoPlayer.PrisonersDilemma ( module GTL.Game.TwoPlayer
                                           , Action1(..), Signal1, u1
                                           , Action2(..), Signal2, u2
                                           ) where


import GTL.Game.TwoPlayer
import Data.Ix (Ix)

data Action1 = C1 | D1 deriving (Show, Bounded, Ix, Eq, Ord)
type Signal1 = Action2

data Action2 = C2 | D2 deriving (Show, Bounded, Ix, Eq, Ord)
type Signal2 = Action1

u1 :: Action1 -> Signal1 -> Double
u1 D1 C2 = 2
u1 C1 C2 = 1
u1 D1 D2 = 0
u1 C1 D2 = -1

u2 :: Action2 -> Signal2 -> Double
u2 D2 C1 = 2
u2 C2 C1 = 1
u2 D2 D1 = 0
u2 C2 D1 = -1
