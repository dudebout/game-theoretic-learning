module GTL.Game.TwoPlayer.BattleOfTheSexes where

import Data.Ix (Ix)

data ActionM = Om | Fm deriving (Show, Bounded, Ix, Eq, Ord)
type SignalM = ActionW

data ActionW = Ow | Fw deriving (Show, Bounded, Ix, Eq, Ord)
type SignalW = ActionM

uM :: ActionM -> SignalM -> Double
uM Fm Fw = 2
uM Om Ow = 1
uM Om Fw = 0
uM Fm Ow = 0

uW :: ActionW -> SignalW -> Double
uW Ow Om = 3
uW Fw Fm = 2
uW Ow Fm = 1
uW Fw Om = 0
