{-# LANGUAGE RecordWildCards, FlexibleInstances, RankNTypes #-}
module Sonic.Constraints
  ( compressMulConstraints
  , rPoly
  , sPoly
  , tPoly
  , kPoly
  )
where

import Protolude hiding (head)
import Data.List (zipWith4, head)
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial (poly, Endianness(..))
import Math.Polynomial.Laurent

import Sonic.Curve (Fr)
import Sonic.Utils

compressMulConstraints :: (Num f) => Assignment f -> f -> f
compressMulConstraints Assignment{..} y = sum $ zipWith4 f aL aR aO [1..]
  where
    f ai bi ci i = (ai * bi - ci) * ((y ^ i) + y ^ (-i))

rPoly
  :: (Eq f, Num f, Show f)
  => Assignment f
  -> BiVariateLaurent f
rPoly Assignment{..} =
  newLaurent
    (negate (2 * n))
    (reorder $ Laurent 0 [] : (concat $ zipWith4 f aL aR aO [1..]))
  where
    f ai bi ci i = [Laurent i [ai], Laurent (-i) [bi], Laurent (-i - n) [ci]]
    reorder = sortBy (\l1 l2 -> compare (expLaurent l1) (expLaurent l2))
    n = length aL

sPoly
  :: (Eq f, Num f)
  => GateWeights f
  -> BiVariateLaurent f
sPoly GateWeights{..}
  = addLaurent
      (addLaurent
        (newLaurent (negate n) (zipWith f (reverse wL) [1..]))
        (newLaurent 1 (zipWith f wR [1..]))
      )
      (newLaurent (n+1) (zipWith g wO [1..]))
  where
    f wi i = newLaurent (n+1) wi
    g wi i = addLaurent
      (addLaurent (newLaurent i [-1]) (newLaurent (-i) [-1]))
      (newLaurent (n+1) wi)
    n = length $ head wL
    --m = length wL
    -- ^ size(wL) = Q x n
    --uYi i = newLaurent (m + 1) (replicate (m-1) (wL !! i))

tPoly
  :: BiVariateLaurent Fr
  -> BiVariateLaurent Fr
  -> Laurent Fr
  -> BiVariateLaurent Fr
tPoly rP sP kP
  -- r(X, 1) * (r(X,Y) + s(X, Y)) - k(Y)
  = addLaurent
      (multLaurent
        (convertToTwoVariateX $ evalOnY 1 rP)
        (addLaurent rP sP)
      )
     (convertToTwoVariateY $ negate kP)

kPoly
  :: (Eq f, Num f)
  => [f]
  -> Int
  -> Laurent f
kPoly k n = newLaurent (n+1) k
