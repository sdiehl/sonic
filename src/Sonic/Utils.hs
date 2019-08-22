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
evalX = (. unPoly) . (sum .) . (<$>) . uncurry . (. fromIntegral) . (scale 0 .) . pow

evalY :: GaloisField k => k -> BiVPoly k -> VPoly k
evalY = (. unPoly) . (toPoly .) . (<$>) . (<$>) . flip eval

fromX :: GaloisField k => VPoly k -> BiVPoly k
fromX = toPoly . ((<$>) . (<$>) . monomial) 0 . unPoly

fromY :: GaloisField k => VPoly k -> BiVPoly k
fromY = monomial 0
