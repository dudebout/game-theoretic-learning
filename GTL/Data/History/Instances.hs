{-# LANGUAGE TemplateHaskell #-}

module GTL.Data.History.Instances ( History0, History1, History2 ) where

import GTL.Data.History.TH
import GTL.Data.Finite ()
import Text.PrettyPrint.Leijen ()

$(buildHistories [0..2])
