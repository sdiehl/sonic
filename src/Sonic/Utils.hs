module Sonic.Utils
  ( BiVPoly
  , evalX
  , evalY
  , fromX
  , fromY
  ) where

import Protolude
import Data.Poly.Laurent (VPoly, eval, monomial, scale, toPoly, unPoly)
import GaloisField (GaloisField(..))

type BiVPoly k = VPoly (VPoly k)

evalX :: GaloisField k => k -> BiVPoly k -> VPoly k
evalX x = sum . (<$>) (uncurry (scale 0 . pow x . fromIntegral)) . unPoly

evalY :: GaloisField k => k -> BiVPoly k -> VPoly k
evalY x = toPoly . ((<$>) . (<$>) . flip eval) x . unPoly

fromX :: GaloisField k => VPoly k -> BiVPoly k
fromX = toPoly . ((<$>) . (<$>) . monomial) 0 . unPoly

fromY :: GaloisField k => VPoly k -> BiVPoly k
fromY = monomial 0
