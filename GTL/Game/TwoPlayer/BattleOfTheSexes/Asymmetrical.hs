{-# LANGUAGE TypeOperators #-}

module GTL.Game.TwoPlayer.BattleOfTheSexes.Asymmetrical ( module GTL.Game.TwoPlayer
                                                        , ActionM(..), SignalM, uM
                                                        , ActionW(..), SignalW, uW
                                                        , Actions, Signals
                                                        ) where

import GTL.Game.TwoPlayer
import GTL.Data.Utility (UtilityAS)
import Data.Ix (Ix)
import Data.HList (HNil, (:*:))

data ActionM = Om | Fm deriving (Show, Bounded, Ix, Eq, Ord)
type SignalM = ActionW

data ActionW = Ow | Fw deriving (Show, Bounded, Ix, Eq, Ord)
type SignalW = ActionM

uM :: UtilityAS ActionM SignalM
uM Fm Fw = 2
uM Om Ow = 1
uM Om Fw = 0
uM Fm Ow = 0

uW :: UtilityAS ActionW SignalW
uW Ow Om = 3
uW Fw Fm = 2
uW Ow Fm = 1
uW Fw Om = 0

type Actions = ActionM :*: ActionW :*: HNil
type Signals = SignalM :*: SignalW :*: HNil
