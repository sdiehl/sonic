{-# LANGUAGE RecordWildCards #-}
module Sonic.CommitmentScheme where

import Protolude
import Data.List ((!!))
import Pairing.Group as Group (G1, G2, GT, g1, g2, expn)
import Pairing.Fr as Fr (Fr, random, frInv, new)
import Pairing.Pairing (reducedPairing)
import Pairing.CyclicGroup (AsInteger(..))
import Pairing.Point (gMul)

import qualified Math.Polynomial as Poly
import Math.Polynomial.Laurent

import Sonic.Utils
import Sonic.SRS

type Commitment = G1
type Opening f = (f, G1)

commitPoly
  :: (AsInteger f, Num f, Eq f, Fractional f)
  => SRS
  -> Integer
  -> f
  -> Laurent f
  -> Commitment
commitPoly SRS{..} maxm x poly
  = gxi `expn` (evalLaurent poly x)
  where
    diff = fromInteger (d - maxm)
    gxi = if diff >= 0
          then gPositiveAlphaX !! (diff - 1)
          else gNegativeAlphaX !! (abs diff - 1)

openPoly
  :: (AsInteger f, Num f, Eq f, Fractional f)
  => SRS
  -> Commitment
  -> f
  -> f
  -> Laurent f
  -> Opening f
openPoly srs _commitment x z fX
  = let fz = evalLaurent fX z
        wPoly = (fX - newLaurent 0 [fz]) `quotLaurent` (newLaurent 0 [-z, 1])
        w = g1 `expn` (evalLaurent wPoly x)
    in (fz, w)

pcV
  :: (AsInteger f, Num f, Eq f, Fractional f)
  => SRS
  -> Integer
  -> Commitment
  -> f
  -> Opening f
  -> Bool
pcV srs@SRS{..} maxm commitment z (v, w)
  = reducedPairing w (hPositiveAlphaX !! 1) -- when i = 1
    <>
    (reducedPairing ((g1 `expn` v) <> (w `expn` (negate z))) (hPositiveAlphaX !! 0)) -- when i = 0
    ==
    (reducedPairing commitment hxi)
  where
    diff = fromInteger (-d + maxm)
    hxi = if diff >= 0
          then hPositiveX !! diff
          else hNegativeX !! (abs diff - 1)

