module GTL.Interaction.Learning where

import GTL.Interaction.Consistency (mockup)
import GTL.Interaction.Optimality (uniformSmoothedOptimalResponse)
import GTL.Data.Mockup (Role, updateMockup, MockupArray, arrayToMockup, mockupToArray)
import GTL.Data.Utility (Epsilon, Unitless)
import GTL.Data.Signaling (SignalingWXAS)

import Data.Ix (Ix)
import GTL.Data.History (History)
import Control.Monad.RWS (RWS, evalRWS, ask, get, put, tell)

data Param w x a s = Param { role     :: Role x a s
                           , signaling :: SignalingWXAS w x a s
                           , epsilon  :: Epsilon
                           , stepSize :: Unitless
                           , timeMax   :: Int }

data Var a s h h' = Var { mockupArray :: !(MockupArray a s h h')
                        , time         :: Int } deriving (Show)

type Comp w x a s h h' = RWS (Param w x a s) [Var a s h h'] (Var a s h h')

runComp :: (Comp w x a s h h') comp -> Param w x a s -> Var a s h h' -> [Var a s h h']
runComp comp param var = snd $ evalRWS comp param var

oneStep :: ( Bounded (h' s), Bounded (h a)
           , Bounded s, Bounded a, Bounded x, Bounded w
           , Ix (h' s), Ix (h a)
           , Ix s, Ix a, Ix x, Ix w
           , History h', History h
           ) => (Comp w x a s h h') ()
oneStep = do
  Param { role     = rol
        , signaling = sig
        , epsilon  = eps
        , stepSize = step } <- ask
  Var { mockupArray = arr
      , time         = t } <- get
  let t'     = t + 1
      mock   = arrayToMockup arr
      resp   = uniformSmoothedOptimalResponse rol eps mock
      mock'  = mockup rol sig resp
      mock'' = updateMockup step mock mock'
      arr'   = mockupToArray mock''
      var    = Var arr' t'
  tell [var]
  put var
