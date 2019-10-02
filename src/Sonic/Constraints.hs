-- Constraint system proposed by Bootle et al.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Sonic.Constraints
  ( rPoly
  , sPoly
  , tPoly
  , kPoly
  ) where

import Protolude hiding (head)
import Bulletproofs.ArithmeticCircuit (Assignment(..), GateWeights(..))
import Data.List (zipWith4, head, (!!))
import Data.Pairing.BLS12381 (Fr)
import Data.Poly.Laurent (VPoly, eval, scale, monomial, toPoly, unPoly, unPoly)
import qualified Data.Vector as V

import Sonic.Utils (BiVPoly, fromX, fromY, evalY)

-- r(X,Y) = \sum_{i=1}^n (a_i X^i Y^i + b_i X^{-i} Y^{-i} + c_i X^{-i-n} Y^{-i-n}
rPoly
  :: (Eq f, Num f)
  => Assignment f
  -> BiVPoly f
rPoly Assignment{..} =
  toPoly . V.fromList $ concat (zipWith4 f aL aR aO [1..])
  where
    f ai bi ci i = [(i, monomial i ai), (-i, monomial (-i) bi), (-i - n, monomial (-i - n) ci)]
    n = length aL

sPoly
  :: forall f. (Eq f, Num f)
  => GateWeights f
  -> BiVPoly f
sPoly GateWeights{..}
  = toPoly . V.fromList . concat $
    (\i -> [(-i, uiY i), (i, viY i), (i + n, wiY i)]) <$> [1..n]
  where
    uiY, viY, wiY :: Int -> VPoly f
    uiY i = xiY i wL
    viY i = xiY i wR
    wiY i = monomial (-i) (-1) + monomial i (-1) + xiY i wO

    xiY :: Int -> [[f]] -> VPoly f
    xiY i xL =  foldl' (fxqi i) 0 (zip [1..] xL)
    fxqi i acc (q, xLq) = acc + monomial (q + n) (xLq !! (i - 1))

    -- n: multiplication constraints
    n :: Int
    n = length $ head wL

tPoly
  :: BiVPoly Fr
  -> BiVPoly Fr
  -> VPoly Fr
  -> BiVPoly Fr
tPoly rXY sXY kY
  -- r(X, 1) * (r(X,Y) + s(X, Y)) - k(Y)
  = (rX1 * rXY') + k1Y
  where
    rXY' = rXY + sXY
    rX1 = fromX $ evalY 1 rXY
    k1Y = fromY $ negate kY

kPoly :: [Fr] -> Int -> VPoly Fr
kPoly k n = toPoly . V.fromList $ zip [n+1..] k
