-- Polynomial commitment scheme inspired by Kate et al.

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Sonic.CommitmentScheme
  ( commitPoly
  , openPoly
  , pcV
  ) where

import Protolude hiding (quot)
import qualified Data.Vector as V
import Data.Curve (Curve(..), mul)
import Data.Euclidean (quot)
import Data.Pairing.BLS12381 (Fr, G1, GT, BLS12381, pairing)
import Data.Poly.Laurent (VPoly, eval, monomial, toPoly, unPoly)
import Sonic.SRS (SRS(..))

type Opening f = (f, G1 BLS12381)

commitPoly :: SRS -> Int -> VPoly Fr -> G1 BLS12381
commitPoly SRS{..} maxm fX
  = foldl' (<>) mempty (negPowers V.++ posPowers)
  where
    difference = srsD - maxm
    powofx = monomial difference 1
    xfX = unPoly (powofx * fX)
    expL = fst (xfX V.! 0)
    coeffsL = snd <$> xfX
    (negCoeffs, posCoeffs)
      = if expL < 0
        then V.splitAt (abs expL) coeffsL
        else ([], coeffsL)
    negPowers = V.zipWith mul gNegativeAlphaX (V.reverse negCoeffs)
    posPowers = V.zipWith mul gPositiveAlphaX posCoeffs

openPoly :: SRS -> G1 BLS12381 -> Fr -> VPoly Fr -> Opening Fr
openPoly SRS{..} _commitment z fX
  = let fz = eval fX z
        wPoly = fX - monomial 0 fz `quot` toPoly [(0, -z), (1, 1)]
        poly = unPoly wPoly
        expL = fst (poly V.! 0)
        coeffsL = snd <$> poly
        (negCoeffs, posCoeffs)
          = if expL < 0
            then V.splitAt (abs expL) coeffsL
            else V.splitAt 0 coeffsL
        negPowers = V.zipWith mul gNegativeX (V.reverse negCoeffs)
        posPowers = V.zipWith mul gPositiveX posCoeffs
        w = foldl' (<>) mempty (negPowers V.++ posPowers)
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
          then hPositiveX V.! difference
          else hNegativeX V.! (abs difference - 1)
    eA, eB, eC :: GT BLS12381
    eA = pairing w (hPositiveAlphaX V.! 1) -- when i = 1
    eB = pairing ((gen `mul` v) <> (w `mul` negate z)) (hPositiveAlphaX V.! 0) -- when i = 0
    eC = pairing commitment hxi
