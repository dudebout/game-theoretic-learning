{-# LANGUAGE TypeOperators #-}

module GTL.Game.TwoPlayer where

import Data.Tuple (swap)
import Data.HList (HNil, (:*:))
import Data.Tuple.HList (toHList, fromHList)

perfectInfo :: (a1, a2) -> (a2, a1)
perfectInfo = swap

perfectInfoH :: (a1 :*: a2 :*: HNil) -> (a2 :*: a1 :*: HNil)
perfectInfoH = toHList . perfectInfo . fromHList
