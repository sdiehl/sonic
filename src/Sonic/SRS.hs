-- Structured reference string with unknowns x and α
-- This string is designed so that g^α is omitted from the reference string.
module Sonic.SRS where

import Protolude
import Data.Pairing (pairing)
import Data.Curve (Curve(..), mul)
import Data.Pairing.BLS12381 (Fr, G1, G2, GT, BLS12381)

data SRS = SRS
  { srsD :: Int
  , gNegativeX :: [G1 BLS12381]
  , gPositiveX :: [G1 BLS12381]
  , hNegativeX :: [G2 BLS12381]
  , hPositiveX :: [G2 BLS12381]
  , gNegativeAlphaX :: [G1 BLS12381]
  , gPositiveAlphaX :: [G1 BLS12381]
  , hNegativeAlphaX :: [G2 BLS12381]
  , hPositiveAlphaX :: [G2 BLS12381]
  , srsPairing :: GT BLS12381
  }

-- | Create a new Structured Reference String (SRS)
-- 'd' should be large enough to support the circuit depth 'n'.
-- In this implementation, 'd' should be greater than (7 * 'n')
new :: Int -> Fr -> Fr -> SRS
new d x alpha
  = let xInv = recip x
    in SRS
        { srsD = d
        , gNegativeX = mul gen . (^) xInv <$> [1..d]
        , gPositiveX = mul gen . (^) x <$> [0..d]
        , hNegativeX = mul gen . (^) xInv <$> [1..d]
        , hPositiveX = mul gen . (^) x <$> [0..d]
        , gNegativeAlphaX = mul gen . ((*) alpha . (^) xInv) <$> [1..d]
        -- g^alpha is not shared
        , gPositiveAlphaX = mul gen 0 : (mul gen . ((*) alpha . (^) x) <$> [1..d])
        , hNegativeAlphaX = mul gen . ((*) alpha . (^) xInv) <$> [1..d]
        , hPositiveAlphaX = mul gen . ((*) alpha . (^) x) <$> [0..d]
        , srsPairing = pairing gen (mul gen alpha)
        }
