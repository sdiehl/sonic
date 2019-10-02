-- Polynomial commitment scheme inspired by Kate et al.
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

-- Commit(info, f(X)) -> F:
commitPoly
  :: SRS           -- srs
  -> Int           -- max
  -> VPoly Fr      -- f(X)
  -> G1 BLS12381   -- F
commitPoly SRS{..} maxm fX
  = foldl'
    (\acc (e, v) -> acc <> if e >= 0
                           then (gPositiveAlphaX V.! (e - 1)) `mul` v           -- {g^{alpha*X^{d-max}*f(X) : X<0}
                           else (gNegativeAlphaX V.! (abs e - 1)) `mul` v       -- {g^{alpha*X^{d-max}*f(X) : X>0}
    ) mempty xfX
  where
    difference = srsD - maxm       -- d-max
    powofx = monomial difference 1 -- X^{d-max}
    xfX = unPoly (powofx * fX)     -- X^{d-max} * f(X)

openPoly
  :: SRS          -- srs
  -> Fr           -- z
  -> VPoly Fr     -- f(X)
  -> Opening Fr   -- (f(z), W)
openPoly SRS{..} z fX = (fz, w)
  where
    fz = eval fX z -- f(z)
    wPoly = unPoly $ (fX - monomial 0 fz) `quot` toPoly (V.fromList [(0, -z), (1, 1)]) -- w(X) = (f(X) - f(z))/(X-z)
    w = foldl'
        (\acc (e, v) -> acc <> if e >= 0
                               then (gPositiveX V.! e) `mul` v           -- {g^{w(X)} : X>0}
                               else (gNegativeX V.! (abs e - 1)) `mul` v -- {g^{w(X)} : X<0}
        ) mempty wPoly

pcV
  :: SRS          -- srs
  -> Int          -- max
  -> G1 BLS12381  -- F
  -> Fr           -- z
  -> Opening Fr   -- (f(z), W)
  -> Bool         -- 0 | 1
pcV SRS{..} maxm commitment z (v, w)
  = eA <> eB == eC
  where
    eA, eB, eC :: GT BLS12381
    eA = pairing w (hPositiveAlphaX V.! 1)                                     -- e(W, h^{alpha*x})
    eB = pairing ((gen `mul` v) <> (w `mul` negate z)) (hPositiveAlphaX V.! 0) -- e(g^v W^{-z}, h^{alpha})
    eC = pairing commitment hxi                                                -- e(F, h^{x^{-d+max}})
    difference = -srsD + maxm                        -- -d+max
    hxi = if difference >= 0                         -- h^{x^{-d+max}}
          then hPositiveX V.! difference
          else hNegativeX V.! (abs difference - 1)
