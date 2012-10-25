{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GTL.Data.HList.HZip3 where

import Data.HList.HListPrelude

class HZip3 x y z l | x y z -> l, l -> x y z where
  hZip3   :: x -> y -> z -> l
  hUnzip3 :: l -> (x, y, z)

instance HZip3 HNil HNil HNil HNil where
  hZip3   HNil HNil HNil = HNil
  hUnzip3 HNil           = (HNil, HNil, HNil)

instance HZip3 tx ty tz l => HZip3 (HCons hx tx) (HCons hy ty) (HCons hz tz) (HCons (hx, hy, hz) l) where
  hZip3   (HCons hx tx) (HCons hy ty) (HCons hz tz) = HCons (hx, hy, hz) (hZip3 tx ty tz)
  hUnzip3 (HCons (hx, hy, hz) l)                    = (HCons hx tx, HCons hy ty, HCons hz tz)
      where (tx, ty, tz) = hUnzip3 l
