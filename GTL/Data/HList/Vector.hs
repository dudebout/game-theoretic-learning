{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module GTL.Data.HList.Vector where

import Data.HList.HListPrelude
import Data.Monoid

newtype VHCons e l = VHCons (HCons e l)
newtype VHNil      = VHNil HNil

class VHList l v | l -> v, v -> l where
    toVector   :: l -> v
    fromVector :: v -> l

instance VHList HNil VHNil where
    toVector                = VHNil
    fromVector (VHNil HNil) = HNil

instance (VHList l v) => VHList (HCons e l) (VHCons e v) where
    toVector   (HCons e l)          = VHCons (HCons e (toVector l))
    fromVector (VHCons (HCons e v)) = HCons e (fromVector v)

instance Monoid VHNil where
    mempty                               = VHNil HNil
    (VHNil HNil) `mappend` (VHNil HNil)  = VHNil HNil

instance (VHList l v, Monoid e, Monoid v) => Monoid (VHCons e v) where
    mempty                                                = VHCons $ HCons mempty mempty
    (VHCons (HCons e l)) `mappend` (VHCons (HCons e' l')) = VHCons $ HCons (e `mappend` e') (l `mappend` l')
