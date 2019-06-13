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
  :: (Show f, AsInteger f, Num f, Eq f, Fractional f)
  => SRS
  -> Integer
  -> Laurent f
  -> Commitment
commitPoly SRS{..} maxm fX
  = foldl' (<>) mempty (negPowers ++ posPowers)
  where
    diff = fromInteger (d - maxm)
    powofx = newLaurent diff [1]
    xfX =  multLaurent powofx fX
    expL = expLaurent xfX
    coeffsL = coeffsLaurent xfX
    -- The constant term should always be 0
    (negCoeffs, zeroCoeff, posCoeffs)
      = if expL < 0
        then (take (abs expL) coeffsL, take 1 $ drop (abs expL) coeffsL, drop (abs expL + 1) coeffsL)
        else (take 0 coeffsL, take 1 coeffsL, drop 1 coeffsL)
    negPowers = zipWith expn gNegativeAlphaX (reverse negCoeffs)
    posPowers = zipWith expn gPositiveAlphaX posCoeffs

openPoly
  :: (Show f, AsInteger f, Num f, Eq f, Fractional f)
  => SRS
  -> Commitment
  -> f
  -> Laurent f
  -> Opening f
openPoly SRS{..} _commitment z fX
  = let fz = evalLaurent fX z
        wPoly = (fX - newLaurent 0 [fz]) `quotLaurent` (newLaurent 0 [-z, 1])
        expL = expLaurent wPoly
        coeffsL = coeffsLaurent wPoly
        (negCoeffs, posCoeffs)
          = if expL < 0
            then splitAt (abs expL) coeffsL
            else splitAt 0 coeffsL
        negPowers = zipWith expn gNegativeX (reverse negCoeffs)
        posPowers = zipWith expn gPositiveX posCoeffs
        w = foldl' (<>) mempty (negPowers ++ posPowers)
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

