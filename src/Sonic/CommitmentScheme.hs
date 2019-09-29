-- Polynomial commitment scheme inspired by Kate et al.

{-# LANGUAGE RecordWildCards #-}
module Sonic.CommitmentScheme
  ( commitPoly
  , openPoly
  , pcV
  ) where

import Protolude
import Data.List ((!!))
import Data.Curve (Curve(..), mul)
import Data.Pairing.BLS12381 (Fr, G1, GT, BLS12381, pairing)
import Math.Polynomial.Laurent
  (Laurent, newLaurent, quotLaurent, evalLaurent, expLaurent, coeffsLaurent)
import Sonic.SRS (SRS(..))

type Opening f = (f, G1 BLS12381)

commitPoly :: SRS -> Int -> Laurent Fr -> G1 BLS12381
commitPoly SRS{..} maxm fX
  = foldl' (<>) mempty (negPowers ++ posPowers)
  where
    difference = srsD - maxm
    powofx = newLaurent difference [1]
    xfX =  powofx * fX
    expL = expLaurent xfX
    coeffsL = coeffsLaurent xfX
    (negCoeffs, posCoeffs)
      = if expL < 0
        then splitAt (abs expL) coeffsL
        else ([], coeffsL)
    negPowers = zipWith mul gNegativeAlphaX (reverse negCoeffs)
    posPowers = zipWith mul gPositiveAlphaX posCoeffs

openPoly :: SRS -> G1 BLS12381 -> Fr -> Laurent Fr -> Opening Fr
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
  -> G1 BLS12381
  -> Fr
  -> Opening Fr
  -> Bool
pcV SRS{..} maxm commitment z (v, w)
  = eA <> eB == eC
  where
    difference = -srsD + maxm
    hxi = if difference >= 0
          then hPositiveX !! difference
          else hNegativeX !! (abs difference - 1)
    eA, eB, eC :: GT BLS12381
    eA = pairing w (hPositiveAlphaX !! 1) -- when i = 1
    eB = pairing ((gen `mul` v) <> (w `mul` negate z)) (hPositiveAlphaX !! 0) -- when i = 0
    eC = pairing commitment hxi
