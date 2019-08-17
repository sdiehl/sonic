{-# LANGUAGE RecordWildCards #-}
module Sonic.CommitmentScheme
  ( commitPoly
  , openPoly
  , pcV
  )
where

import Protolude
import Data.List ((!!))
import Pairing.Pairing (reducedPairing)
import Math.Polynomial

import qualified Math.Polynomial as Poly
import Math.Polynomial.Laurent
import PrimeField
import Text.PrettyPrint.Leijen.Text as PP (pretty, Pretty(..))

import Curve (Curve(..))
import Sonic.Utils
import Sonic.SRS
import Sonic.Curve (Fr, G1, gG1, gG2)

type Opening f = (f, G1)

commitPoly
  :: SRS
  -> Int
  -> Laurent Fr
  -> G1
commitPoly SRS{..} maxm fX
  = foldl' (<>) mempty (negPowers ++ posPowers)
  where
    diff = srsD - maxm
    powofx = newLaurent diff [1]
    xfX =  multLaurent powofx fX
    expL = expLaurent xfX
    coeffsL = coeffsLaurent xfX
    (negCoeffs, posCoeffs)
      = if expL < 0
        then splitAt (abs expL) coeffsL
        else ([], coeffsL)
    negPowers = zipWith mul gNegativeAlphaX (reverse negCoeffs)
    posPowers = zipWith mul gPositiveAlphaX posCoeffs

openPoly
  :: SRS
  -> G1
  -> Fr
  -> Laurent Fr
  -> Opening Fr
openPoly SRS{..} _commitment z fX
  = let fz = evalLaurent fX z
        wPoly = (fX - newLaurent 0 [fz]) `quotLaurent` (newLaurent 0 [-z, 1])
        expL = expLaurent wPoly
        coeffsL = coeffsLaurent wPoly
        (negCoeffs, posCoeffs)
          = if expL < 0
            then splitAt (abs expL) coeffsL
            else splitAt 0 coeffsL
        negPowers = zipWith mul gNegativeX (reverse negCoeffs)
        posPowers = zipWith mul gPositiveX posCoeffs
        w = foldl' (<>) mempty (negPowers ++ posPowers)
    in (fz, w)

pcV
  :: SRS
  -> Int
  -> G1
  -> Fr
  -> Opening Fr
  -> Bool
pcV srs@SRS{..} maxm commitment z (v, w)
  = reducedPairing w (hPositiveAlphaX !! 1) -- when i = 1
    <>
    reducedPairing ((gG1 `mul` v) <> (w `mul` (negate z))) (hPositiveAlphaX !! 0) -- when i = 0
    ==
    (reducedPairing commitment hxi)
  where
    diff = -srsD + maxm
    hxi = if diff >= 0
          then hPositiveX !! diff
          else hNegativeX !! (abs diff - 1)

