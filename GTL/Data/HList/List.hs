{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module GTL.Data.HList.List
    ( transposeL, transposeH
    , WrapInList(..), Head(..), Tail(..), HasTail(..)
    ) where

import Data.HList.HListPrelude
import GTL.Data.HList.Vector (VHList, toVector, fromVector)
import Data.Monoid (Monoid, mempty, (<>))

transposeL :: (HMap WrapInList xh lh, Monoid v, VHList lh v) => [xh] -> lh
transposeL []     = fromVector $ mempty
transposeL (x:xs) = fromVector (toVector headH <> toVector tailH)
    where headH = hMap WrapInList x
          tailH = transposeL xs

data WrapInList = WrapInList
instance Apply WrapInList a [a]
    where apply _ = (:[])


transposeH :: (HMap Tail l l, HMap Head l a, HFoldr HasTail Bool l Bool) => l -> [a]
transposeH l = case (hasTail l) of True  -> (hMap Head l):(transposeH $ hMap Tail l)
                                   False -> []

hasTail :: (HFoldr HasTail Bool l Bool) => l -> Bool
hasTail = hFoldr HasTail True

data Head = Head
instance Apply Head [a] a
    where apply _ = head

data Tail = Tail
instance Apply Tail [a] [a]
    where apply _ = tail

data HasTail = HasTail
instance Apply HasTail ([a], Bool) Bool
    where apply _ (list, otherHaveTails) = and [not $ null list, otherHaveTails]
