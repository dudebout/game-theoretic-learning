{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GTL.PrettyPrint where

import Text.PrettyPrint.Leijen (Pretty, pretty, tupled, vcat, hsep, align, Doc, text, (<>), rbrace, lbrace, space, encloseSep, empty, semi)
import Text.Printf (printf)
import Data.Ix (Ix)
import Data.HList (HMapOut, hMapOut, Apply, apply)
import GTL.Data.Finite (rangeF)
import GTL.Data.Mockup (MockupState(..), ExogenousMockupState)
import GTL.Data.History (History(..))
import GTL.Numeric.Probability (Dist)
import Numeric.Probability.Distribution (decons, norm)

arrow :: Doc
arrow = text "->"

instance (Ix a, Bounded a, Pretty a, Pretty b) => Pretty (a -> b) where
    pretty fun = case rangeF of
                   a:[] -> pretty $ fun a
                   _    -> vcat [hsep [pretty a, arrow, align $ pretty $ fun a] | a <- rangeF]

instance (Pretty (h a), Pretty (h' s)) => Pretty (MockupState a s h h') where
    pretty (MockupState ha hs) = tupled [pretty ha, pretty hs]

instance (Pretty (h s)) => Pretty (ExogenousMockupState a s h) where
    pretty (MockupState _ hs) = pretty hs

instance (Pretty a, History h) => Pretty (h a) where
    pretty h = pretty $ case (fromHistory h) of
               a:[] -> pretty a
               as   -> pretty as

trunc:: Int -> Double -> Doc
trunc precision = text . printf ("%." ++ show precision ++ "f")

distrib :: Doc
distrib = text "~"

instance (Ix a, Bounded a, Pretty a, Ord a) => Pretty (Dist a) where
    pretty = probaList . map (\(x, p) -> probaPair [pretty x, trunc 4 p]) . decons . norm
        where probaPair = encloseSep empty empty distrib
              probaList = encloseSep  (lbrace <> space) (space <> rbrace) (semi <> space)

instance Show a => Pretty a where
    pretty = text . show

data PrettyH = PrettyH
instance Pretty a => Apply PrettyH a Doc where apply _ x = pretty x

hPretty :: HMapOut PrettyH a Doc => a -> [Doc]
hPretty = hMapOut PrettyH
