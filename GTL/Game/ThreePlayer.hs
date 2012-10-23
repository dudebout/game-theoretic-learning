{-# LANGUAGE TypeOperators #-}

module GTL.Game.ThreePlayer (perfectInfo) where

import Data.HList (HNil, (:*:))
import Data.Tuple.HList (toHList, fromHList)

perfectInfo :: (a1 :*: a2 :*: a3 :*: HNil) -> ((a2, a3) :*: (a1, a3) :*: (a1, a2) :*: HNil)
perfectInfo = toHList . perfectInfo' . fromHList

perfectInfo' :: (a1, a2, a3) -> ((a2, a3), (a1, a3), (a1, a2))
perfectInfo' (a1, a2, a3) = ((a2, a3), (a1, a3), (a1, a2))
