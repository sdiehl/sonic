-- Structured reference string with unknowns x and α
-- This string is designed so that g^α is omitted from the reference string.
module Sonic.SRS where

import Protolude
import qualified Data.Vector as V
import Data.Field.Galois (pow)
import Data.Curve (Curve(..), mul)
import Data.Pairing.BLS12381 (Fr, G1, G2, GT, BLS12381, pairing)

data SRS = SRS
  { srsD :: Int
  , gNegativeX :: V.Vector (G1 BLS12381)
  , gPositiveX :: V.Vector (G1 BLS12381)
  , hNegativeX :: V.Vector (G2 BLS12381)
  , hPositiveX :: V.Vector (G2 BLS12381)
  , gNegativeAlphaX :: V.Vector (G1 BLS12381)
  , gPositiveAlphaX :: V.Vector (G1 BLS12381)
  , hNegativeAlphaX :: V.Vector (G2 BLS12381)
  , hPositiveAlphaX :: V.Vector (G2 BLS12381)
  , srsPairing :: GT BLS12381
  }

-- | Create a new Structured Reference String (SRS)
-- 'd' should be large enough to support the circuit depth 'n'.
-- In this implementation, 'd' should be greater than (7 * 'n')
new :: Int -> Fr -> Fr -> SRS
new n x alpha
  = let xInv = recip x
        d = fromIntegral n :: Integer
    in SRS
       { srsD = n                                                                 -- d
       , gNegativeX = mul gen . pow xInv <$> V.fromList [1..d]                    -- {g^{x^i}}_{i=-d}^{-1}
       , gPositiveX = mul gen . pow x <$> V.fromList [0..d]                       -- {g^{x^i}}_{i=0}^d
       , hNegativeX = mul gen . pow xInv <$> V.fromList [1..d]                    -- {h^{x^i}}_{i=-d}^{-1}
       , hPositiveX = mul gen . pow x <$> V.fromList [0..d]                       -- {h^{x^i}}_{i=0}^d
       , gNegativeAlphaX = mul gen . (*) alpha . pow xInv <$> V.fromList [1..d]   -- {g^{alpha*x^i}_{i=-d}^{-1}
       -- g^alpha is not shared
       , gPositiveAlphaX = mul gen . (*) alpha . pow x <$> V.fromList [1..d]      -- {g^{alpha*x^i}}_{i=1}^d
       , hNegativeAlphaX = mul gen . (*) alpha . pow xInv <$> V.fromList [1..d]   -- {h^{alpha*x^i}_{i=-d}^{-1}
       , hPositiveAlphaX = mul gen . (*) alpha . pow x <$> V.fromList [0..d]      -- {h^{alpha*x^i}}_{i=0}^d
       , srsPairing = pairing gen (mul gen alpha)                                 -- e(g, h^{alpha})
       }
