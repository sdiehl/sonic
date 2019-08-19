-- Constraint system proposed by Bootle et al.

{-# LANGUAGE RecordWildCards #-}
module Sonic.Constraints
  ( rPoly
  , sPoly
  , tPoly
  , kPoly
  )
where

import Protolude hiding (head)
import Data.List (zipWith4, head, (!!))
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent

import Sonic.Curve (Fr)
import Sonic.Utils

rPoly
  :: (Eq f, Num f)
  => Assignment f
  -> BiVariateLaurent f
rPoly Assignment{..} =
  newLaurent
    (negate (2 * n))
    (reorder $ newLaurent 0 [] : concat (zipWith4 f aL aR aO [1..]))
  where
    f ai bi ci i = [Laurent i [ai], Laurent (-i) [bi], Laurent (-i - n) [ci]]
    reorder = sortBy (\l1 l2 -> compare (expLaurent l1) (expLaurent l2))
    n = length aL

sPoly
  :: forall f. (Eq f, Num f)
  => GateWeights f
  -> BiVariateLaurent f
sPoly GateWeights{..}
  = foldl'
    (\acc i -> acc
      `addLaurent` newLaurent (-i) [uiY i]
      `addLaurent` newLaurent i [viY i]
      `addLaurent` newLaurent (i + n) [wiY i]
    ) zeroLaurent [1..n]
  where
    uiY, viY, wiY :: Int -> Laurent f
    uiY i = xiY i wL
    viY i = xiY i wR
    wiY i = newLaurent (-i) [-1] `addLaurent` newLaurent i [-1] `addLaurent` xiY i wO

    xiY :: Int -> [[f]] -> Laurent f
    xiY i xL =  foldl' (fxqi i) zeroLaurent (zip [1..] xL)
    fxqi i acc (q, xLq) = acc `addLaurent` newLaurent (q + n) [xLq !! (i - 1)]

    -- n: multiplication constraints
    n = length $ head wL

tPoly
  :: BiVariateLaurent Fr
  -> BiVariateLaurent Fr
  -> Laurent Fr
  -> BiVariateLaurent Fr
tPoly rXY sXY kY
  -- r(X, 1) * (r(X,Y) + s(X, Y)) - k(Y)
  = (rX1 `multLaurent` rXY') `addLaurent` k1Y
  where
    rXY' = rXY `addLaurent` sXY
    rX1 = convertToTwoVariateX $ evalOnY 1 rXY
    k1Y = convertToTwoVariateY $ negate kY

kPoly :: [Fr] -> Int -> Laurent Fr
kPoly k n = newLaurent (n+1) k
