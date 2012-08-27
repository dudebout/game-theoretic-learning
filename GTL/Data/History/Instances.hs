{-# LANGUAGE TemplateHaskell #-}

module GTL.Data.History.Instances ( History0, History1, History2 ) where

import GTL.Data.History.TH

$(buildHistories [0..2])
