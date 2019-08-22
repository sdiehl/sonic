-- Structured reference string with unknowns x and α
-- This string is designed so that g^α is omitted from the reference string.
module Sonic.SRS where

import Protolude
import Curve (Curve(..), Group(..))
import qualified Data.Vector as V
import GaloisField (pow)
import Pairing.Pairing (reducedPairing)

import Sonic.Curve (Fr, G1, G2, GT)

data SRS = SRS
  { srsD :: Int
  , gNegativeX :: V.Vector G1
  , gPositiveX :: V.Vector G1
  , hNegativeX :: V.Vector G2
  , hPositiveX :: V.Vector G2
  , gNegativeAlphaX :: V.Vector G1
  , gPositiveAlphaX :: V.Vector G1
  , hNegativeAlphaX :: V.Vector G2
  , hPositiveAlphaX :: V.Vector G2
  , srsPairing :: GT
  }

-- | Create a new Structured Reference String (SRS)
-- 'd' should be large enough to support the circuit depth 'n'.
-- In this implementation, 'd' should be greater than (7 * 'n')
new :: Int -> Fr -> Fr -> SRS
new n x alpha
  = let xInv = recip x
        d = fromIntegral n :: Integer
    in SRS
        { srsD = n
        , gNegativeX = mul gen . pow xInv <$> V.fromList [1..d]
        , gPositiveX = mul gen . pow x <$> V.fromList [0..d]
        , hNegativeX = mul gen . pow xInv <$> V.fromList [1..d]
        , hPositiveX = mul gen . pow x <$> V.fromList [0..d]
        , gNegativeAlphaX = mul gen . (*) alpha . pow xInv <$> V.fromList [1..d]
        -- g^alpha is not shared
        , gPositiveAlphaX = V.cons (mul gen 0) (mul gen . (*) alpha . pow x <$> V.fromList [1..d])
        , hNegativeAlphaX = mul gen . (*) alpha . pow xInv <$> V.fromList [1..d]
        , hPositiveAlphaX = mul gen . (*) alpha . pow x <$> V.fromList [0..d]
        , srsPairing = reducedPairing gen (mul gen alpha)
        }
