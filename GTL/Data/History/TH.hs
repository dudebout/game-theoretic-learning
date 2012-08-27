{-# LANGUAGE TemplateHaskell, FlexibleInstances, StandaloneDeriving #-}

module GTL.Data.History.TH where

import Language.Haskell.TH
import GTL.Data.History
import Data.Ix (Ix(..))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Text.PrettyPrint.Leijen (Pretty, pretty)

catDecsQ :: [Q [Dec]] -> Q [Dec]
catDecsQ = (liftM concat) . sequence

-- instance (History h, Bounded a) => Bounded (h a)
--     where minBound = undefined
--           maxBound = undefined
-- instance (History h, Eq a)  => Eq (h a)
-- instance (History h, Ord a) => Ord (h a)
-- instance (History h, Ix a)  => Ix (h a)
--     where range   = undefined
--           inRange = undefined

buildHistories :: [Integer] -> Q [Dec]
buildHistories = catDecsQ . (map buildHistory)

buildHistory :: Integer -> Q [Dec]
buildHistory num = do
  let typeName = "History" ++ show num
      deconsName = "fromHistory" ++ show num
      cons = mkName typeName
      decons = mkName deconsName
      len = litE $ IntegerL num
  catDecsQ [ createHistoryType cons decons
           , instantiateBounded cons len
           , instantiateIx cons decons len
           , instantiateHistory cons decons len typeName num]

-- newtype History<k> a = History<k> { fromHistory<k> :: [a] }
createHistoryType :: Name -> Name -> Q [Dec]
createHistoryType cons decons = do
  parameter <- newName "a"
  sequence [newtypeD (cxt []) cons [PlainTV parameter] (recC cons [varStrictType decons $ strictType notStrict (appT listT (varT parameter))]) [''Show, ''Eq, ''Ord]]

instantiateBounded :: Name -> ExpQ -> Q [Dec]
instantiateBounded cons len = [d|instance Bounded a => Bounded ($(conT cons) a) where
                                    minBound = $(conE cons) $ take $len $ repeat minBound
                                    maxBound = $(conE cons) $ take $len $ repeat maxBound|]

instantiateIx :: Name -> Name -> ExpQ -> Q [Dec]
instantiateIx cons decons len =
  [d|instance Ix a => Ix ($(conT cons) a) where
        range (minB, maxB) = map $(conE cons) $ finiteRange ($(varE decons) minB, $(varE decons) maxB)
            where finiteRange = sequence . (map range) . (map (uncurry (,))) . (uncurry zip)
        inRange (minB, maxB) idx = elem idx $ range (minB, maxB)
        index (minB, maxB) idx = fromJust $ elemIndex idx $ range (minB, maxB)|]

instantiateHistory ::  Name -> Name -> ExpQ -> String -> Integer -> Q [Dec]
instantiateHistory cons decons len typeName num =
  [d|instance History $(conT cons) where
        toHistory as | actual == $len = $(conE cons) as
                     | otherwise      = error wrongSize
            where actual = length as
                  wrongSize = "A " ++ typeName ++ " must be constructed from a list of length: "
                              ++ show (num :: Integer) ++ ". Given length: " ++ show actual ++ "."
        fromHistory = $(varE decons)|]
