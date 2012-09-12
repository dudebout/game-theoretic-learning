module GTL.Data.Signaling ( SignalingWXAS, SignalingWXAS2
                          , SignalingXAS2, SignalingwXAS2, toSignalingXAS2, toSignalingwXAS2
                          , SignalingAS2, SignalingwxAS2, toSignalingAS2, toSignalingwxAS2 ) where

import GTL.Numeric.Probability (Dist)

type SignalingWXAS w x a s = w -> x -> a -> Dist (w, s)

type SignalingWXAS2 w x1 a1 s1 x2 a2 s2 = w -> x1 -> a1 -> x2 -> a2 -> Dist (w, s1, s2)

-- No individual state
type SignalingXAS2 x1 a1 s1 x2 a2 s2 = x1 -> a1 -> x2 -> a2 -> Dist (s1, s2)
type SignalingwXAS2 x1 a1 s1 x2 a2 s2 = SignalingWXAS2 () x1 a1 s1 x2 a2 s2

toSignalingXAS2 :: SignalingwXAS2 x1 a1 s1 x2 a2 s2 -> SignalingXAS2 x1 a1 s1 x2 a2 s2
toSignalingXAS2 sig x1 a1 x2 a2 = do
  ((), s1, s2) <- sig () x1 a1 x2 a2
  return (s1, s2)

toSignalingwXAS2 :: SignalingXAS2 x1 a1 s1 x2 a2 s2 -> SignalingwXAS2 x1 a1 s1 x2 a2 s2
toSignalingwXAS2 sig () x1 a1 x2 a2 = do
  (s1, s2) <- sig x1 a1 x2 a2
  return ((), s1, s2)

-- No individual state, no signal state
type SignalingAS2 a1 s1 a2 s2 = a1 -> a2 -> Dist (s1, s2)
type SignalingwxAS2 a1 s1 a2 s2 = SignalingWXAS2 () () a1 s1 () a2 s2

toSignalingAS2 :: SignalingwxAS2 a1 s1 a2 s2 -> SignalingAS2 a1 s1 a2 s2
toSignalingAS2 sig a1 a2 = do
  ((), s1, s2) <- sig () () a1 () a2
  return (s1, s2)

toSignalingwxAS2 :: SignalingAS2 a1 s1 a2 s2 -> SignalingwxAS2 a1 s1 a2 s2
toSignalingwxAS2 sig () () a1 () a2 = do
  (s1, s2) <- sig a1 a2
  return ((), s1, s2)
