{-# LANGUAGE TypeOperators #-}

module GTL.Game.TwoPlayer (perfectInfo) where

import Data.Tuple (swap)
import Data.HList (HNil, (:*:))
import Data.Tuple.HList (toHList, fromHList)

perfectInfo :: (a1 :*: a2 :*: HNil) -> (a2 :*: a1 :*: HNil)
perfectInfo = toHList . perfectInfo' . fromHList

perfectInfo' :: (a1, a2) -> (a2, a1)
perfectInfo' = swap
