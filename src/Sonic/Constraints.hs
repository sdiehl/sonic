{-# LANGUAGE RecordWildCards, FlexibleInstances, OverlappingInstances, RankNTypes #-}
module Sonic.Constraints where

import Protolude hiding (head)
import Data.List (zipWith4, head)
import Pairing.CyclicGroup (CyclicGroup(..))
import Pairing.Fr as Fr (Fr, new)
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial (poly, Endianness(..))
import Math.Polynomial.Laurent
import Sonic.Utils

compressMulConstraints :: Assignment Fr -> Fr -> Fr
compressMulConstraints Assignment{..} y = sum $ zipWith4 f aL aR aO [1..]
  where
    f ai bi ci i = (ai * bi - ci) * ((y ^ i) + y ^ (-i))

rPoly
  :: (Eq f, Num f)
  => Assignment f
  -> Laurent (Laurent f)
rPoly Assignment{..} =
  newLaurent (negate (2 * n)) (reorder $ newLaurent 0 [] : (concat $ zipWith4 f aL aR aO [1..]))
  where
    f ai bi ci i = [newLaurent i [ai], newLaurent (-i) [bi], newLaurent (-i - n) [ci]]
    reorder = sortBy (\l1 l2 -> compare (expLaurent l1) (expLaurent l2))
    n = length aL

sPoly
  :: (Eq f, Num f)
  => GateWeights f
  -> Laurent (Laurent f)
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
    -- ^ size(wL) = Q x n

tPoly
  :: (Eq f, Num f, Fractional f)
  => Laurent (Laurent f)
  -> Laurent (Laurent f)
  -> Laurent f
  -> Laurent (Laurent f)
tPoly rP sP kP
  -- r(X, 1) * (r(X,Y) + s(X, Y)) - k(Y)
  = addLaurent
      (multLaurent
        (convertToTwoVariateX $ evalOnY 1 rP)
        (addLaurent rP sP)
      )
     (convertToTwoVariateY $ negate kP)

polyK
  :: (Eq f, Num f)
  => [f]
  -> Int
  -> Laurent f
polyK k n = newLaurent (n+1) k
