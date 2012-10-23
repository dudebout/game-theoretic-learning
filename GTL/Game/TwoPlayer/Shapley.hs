module GTL.Game.TwoPlayer.Shapley where

import Data.Ix (Ix)

data Action1 = U | M | D deriving (Show, Bounded, Ix, Eq, Ord)
type Signal1 = Action2

data Action2 = L | C | R deriving (Show, Bounded, Ix, Eq, Ord)
type Signal2 = Action1

u1 :: Action1 -> Signal1 -> Double
u1 D L = 1
u1 U C = 1
u1 M R = 1
u1 _ _ = 0

u2 :: Action2 -> Signal2 -> Double
u2 R U = 1
u2 L M = 1
u2 C D = 1
u2 _ _ = 0
