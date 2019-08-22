-- Implementation over Barreto-Naehrig curve

module Sonic.Curve
  ( Fr
  , G1
  , G2
  , GT
  ) where

import qualified Curve.Weierstrass.BN254 as BN254 (Fr, PA)
import qualified Curve.Weierstrass.BN254T as BN254T (PA)
import qualified Group.Field.BN254TF as BN254TF (P)

-- | Prime field @Fr@.
type Fr = BN254.Fr

-- | G1 is @E(Fq)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.PA

-- | G2 is @E'(Fq2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.PA

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fq12@.
type GT = BN254TF.P
