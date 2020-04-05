module Sonic.Utils
  ( BiVLaurent
  , evalX
  , evalY
  , fromX
  , fromY
  ) where

import Protolude
import Data.Poly.Sparse (toPoly, unPoly)
import Data.Poly.Sparse.Laurent (VLaurent, eval, monomial, scale, unLaurent, toLaurent)
import Data.Field.Galois (GaloisField(..), pow)
import qualified GHC.Exts

type BiVLaurent k = VLaurent (VLaurent k)

evalX :: GaloisField k => k -> BiVLaurent k -> VLaurent k
evalX x = sum . fmap (uncurry (scale 0 . pow x . fromIntegral)) . GHC.Exts.toList

evalY :: GaloisField k => k -> BiVLaurent k -> VLaurent k
evalY x = uncurry toLaurent . fmap (toPoly . ((<$>) . (<$>) . flip eval) x . unPoly) . unLaurent

fromX :: GaloisField k => VLaurent k -> BiVLaurent k
fromX = uncurry toLaurent . fmap (toPoly . ((<$>) . (<$>) . monomial) 0 . unPoly) . unLaurent

fromY :: GaloisField k => VLaurent k -> BiVLaurent k
fromY = monomial 0
