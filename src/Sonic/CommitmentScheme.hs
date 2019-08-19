-- Polynomial commitment scheme inspired by Kate et al.

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

import Math.Polynomial.Laurent

import Curve (Curve(..))
import Sonic.SRS
import Sonic.Curve (Fr, G1, gG1)

type Opening f = (f, G1)

commitPoly
  :: SRS
  -> Int
  -> Laurent Fr
  -> G1
commitPoly SRS{..} maxm fX
  = foldl' (<>) mempty (negPowers ++ posPowers)
  where
    difference = srsD - maxm
    powofx = newLaurent difference [1]
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
        wPoly = (fX - newLaurent 0 [fz]) `quotLaurent` newLaurent 0 [-z, 1]
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
pcV SRS{..} maxm commitment z (v, w)
  = reducedPairing w (hPositiveAlphaX !! 1) -- when i = 1
    <>
    reducedPairing ((gG1 `mul` v) <> (w `mul` negate z)) (hPositiveAlphaX !! 0) -- when i = 0
    ==
    reducedPairing commitment hxi
  where
    difference = -srsD + maxm
    hxi = if difference >= 0
          then hPositiveX !! difference
          else hNegativeX !! (abs difference - 1)

