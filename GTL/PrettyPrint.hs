{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances #-}

module GTL.PrettyPrint where

import Text.PrettyPrint.Leijen (Pretty, pretty, tupled, vcat, hsep, align, Doc, text)
import Data.Ix (Ix)
import GTL.Data.Finite (rangeF)
import GTL.Data.Mockup (MockupState(..), ExogenousMockupState)
import GTL.Data.History

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
