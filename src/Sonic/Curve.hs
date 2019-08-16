-- Implementation over Barreto-Naehrig curve
module Sonic.Curve where

import Protolude
import qualified Curve.Weierstrass.BN254 as BN254
import qualified Curve.Weierstrass.BN254T as BN254T
import qualified Group.Field.BN254TF as BN254TF
-------------------------------------------------------------------------------
-- Galois fields
-------------------------------------------------------------------------------

-- | Prime field @Fq@.
type Fq = BN254.Fq

-- | Quadratic extension field of @Fq@ defined as @Fq2 = Fq[u]/<u^2 + 1>@.
type Fq2 = BN254T.Fq2

-- | Cubic extension field of @Fq2@ defined as @Fq6 = Fq2[v]/<v^3 - (9 + u)>@.
type Fq6 = BN254TF.Fq6

-- | Quadratic extension field of @Fq6@ defined as @Fq12 = Fq6[w]/<w^2 - v>@.
type Fq12 = BN254TF.Fq12

-- | Prime field @Fr@.
type Fr = BN254.Fr

-------------------------------------------------------------------------------
-- Elliptic curves
-------------------------------------------------------------------------------

-- | G1 is @E(Fq)@ defined by @y^2 = x^3 + b@.
type G1 = BN254.PA

-- | G2 is @E'(Fq2)@ defined by @y^2 = x^3 + b / xi@.
type G2 = BN254T.PA

-- | G2' is G2 in Jacobian coordinates.
type G2' = BN254T.PJ

-- | GT is subgroup of @r@-th roots of unity of the multiplicative group of @Fq12@.
type GT = BN254TF.P

-- | Generator of G1.
gG1 :: G1
gG1 = BN254.gA

-- | Generator of G2.
gG2 :: G2
gG2 = BN254T.gA

-- | Generator of GT.
gGT :: GT
gGT = BN254TF.g_

-- | Order of G1.
rG1 :: Integer
rG1 = BN254._r

-- | Order of G2.
rG2 :: Integer
rG2 = BN254T._r

-- | Order of GT.
rGT :: Integer
rGT = BN254TF._r
