{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GTL.Data.HList.HZip4 where

import Data.HList.HListPrelude

class HZip4 w x y z l | w x y z -> l, l -> w x y z where
  hZip4   :: w -> x -> y -> z -> l
  hUnzip4 :: l -> (w, x, y, z)

instance HZip4 HNil HNil HNil HNil HNil where
  hZip4   HNil HNil HNil HNil = HNil
  hUnzip4 HNil                = (HNil, HNil, HNil, HNil)

instance HZip4 tw tx ty tz l => HZip4 (HCons hw tw) (HCons hx tx) (HCons hy ty) (HCons hz tz) (HCons (hw, hx, hy, hz) l) where
  hZip4   (HCons hw tw) (HCons hx tx) (HCons hy ty) (HCons hz tz) = HCons (hw, hx, hy, hz) (hZip4 tw tx ty tz)
  hUnzip4 (HCons (hw, hx, hy, hz) l)                              = (HCons hw tw, HCons hx tx, HCons hy ty, HCons hz tz)
      where (tw, tx, ty, tz) = hUnzip4 l
