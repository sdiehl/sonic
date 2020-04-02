-- Constraint system proposed by Bootle et al.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Sonic.Constraints
  ( rPoly
  , sPoly
  , tPoly
  , kPoly
  ) where

import Protolude hiding (head, Semiring)
import Bulletproofs.ArithmeticCircuit (Assignment(..), GateWeights(..))
import Data.List (zipWith4, head, (!!))
import Data.Pairing.BLS12381 (Fr)
import Data.Poly.Sparse.Laurent (VLaurent, monomial)
import Data.Semiring (Semiring)
import qualified GHC.Exts

import Sonic.Utils (BiVLaurent, fromX, fromY, evalY)

-- r(X,Y) = \sum_{i=1}^n (a_i X^i Y^i + b_i X^{-i} Y^{-i} + c_i X^{-i-n} Y^{-i-n})
rPoly
  :: (Eq f, Semiring f)
  => Assignment f
  -> BiVLaurent f
rPoly Assignment{..} =
  GHC.Exts.fromList $ concat (zipWith4 f aL aR aO [1..])
  where
    f ai bi ci i = [(i, monomial i ai), (-i, monomial (-i) bi), (-i - n, monomial (-i - n) ci)]
    n = length aL

-- s(X,Y) = \sum_{i=1}^n (u_i(Y)X^{-i} + v_i(Y)X^i + w_i(Y)X^{i+n})
sPoly
  :: forall f. (Eq f, Num f, Semiring f)
  => GateWeights f
  -> BiVLaurent f
sPoly GateWeights{..}
  = GHC.Exts.fromList . concat $
    (\i -> [(-i, uiY i), (i, viY i), (i + n, wiY i)]) <$> [1..n]
  where
    uiY, viY, wiY :: Int -> VLaurent f
    uiY i = xiY i wL  -- u_i(Y) = \sum_{q=1}^Q (Y^{q+n} u_{q,i})
    viY i = xiY i wR  -- v_i(Y) = \sum_{q=1}^Q (Y^{q+n} v_{q,i}
    wiY i = monomial (-i) (-1) + monomial i (-1) + xiY i wO -- w_i(Y) = -Y^{i} - Y^{-i} + \sum_{q=1}^Q (Y^{q+n} u_{q,i}

    xiY :: Int -> [[f]] -> VLaurent f
    xiY i xL = foldl' (fxqi i) 0 (zip [1..] xL)
    fxqi i acc (q, xLq) = acc + monomial (q + n) (xLq !! (i - 1))

    -- n: multiplication constraints
    n :: Int
    n = length $ head wL

-- t(X,Y) = r(X, 1)(r(X,Y) + s(X, Y)) - k(Y)
tPoly
  :: BiVLaurent Fr
  -> BiVLaurent Fr
  -> VLaurent Fr
  -> BiVLaurent Fr
tPoly rXY sXY kY = (rX1 * rXY') + k1Y
  where
    rXY' = rXY + sXY
    rX1 = fromX $ evalY 1 rXY
    k1Y = fromY $ negate kY

kPoly :: [Fr] -> Int -> VLaurent Fr
kPoly k n = GHC.Exts.fromList $ zip [n+1..] k
