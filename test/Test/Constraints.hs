{-# LANGUAGE TypeApplications #-}
module Test.Constraints where

import Protolude
import Bulletproofs.ArithmeticCircuit
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (rnd)
import Data.List ((!!))
import Data.Pairing.BLS12381
import Data.Poly.Sparse.Laurent (eval)
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import Sonic.Utils
import Sonic.Constraints
import Test.Reference

-- a·uq + b·vq + c·wq = kq
prop_linear_constraints :: Property
prop_linear_constraints = QCM.monadicIO $ do
  (acircuit@ArithCircuit{..}, assignment@Assignment{..}) <- lift . generate $ rndCircuit
  let GateWeights{..} = weights
      assertions = zipWith
        (\i csq -> aL `dot` (wL !! i) + aR `dot` (wR !! i) + aO `dot` (wO !! i) == csq) [0..] cs
  pure $ and assertions === True
  where
    dot a b = sum $ zipWith (*) a b

-- r(X, Y) = r(XY, 1)
prop_rPoly_prop :: Fr -> Fr -> Property
prop_rPoly_prop x y = QCM.monadicIO $ do
  assignment <- lift $ generate $ arithAssignmentGen 3
  let rP = rPoly assignment
  pure $ eval (evalY y rP) x === eval (evalY 1 rP) (x * y)

-- Constant term in polynomial r[X, Y] is zero
prop_rPoly_zero_constant :: Fr -> Fr -> Property
prop_rPoly_zero_constant x y = QCM.monadicIO $ do
  aL <- QCM.run $ replicateM 10 rnd
  aR <- QCM.run $ replicateM 10 rnd
  let aO = zipWith (*) aL aR
      rXY = rPoly @Fr (Assignment aL aR aO)
  r <- QCM.run rnd
  pure $ 0 === maybe 0 (`eval` r) (getZeroCoeff rXY)

-- Constant term in polynomial s[X, Y] is zero
prop_sPoly_zero_constant :: Fr -> Fr -> Property
prop_sPoly_zero_constant x y = QCM.monadicIO $ do
  (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
  let sXY = sPoly weights
  r <- lift rnd
  pure $ 0 === maybe 0 (`eval` r) (getZeroCoeff sXY)

-- Constant term in polynomial (r[X, Y] + s[X, Y]) is zero
prop_sPoly_plus_rPoly_zero_constant :: Fr -> Fr -> Property
prop_sPoly_plus_rPoly_zero_constant x y = QCM.monadicIO $ do
  (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
  let rXY = rPoly assignment
      sXY = sPoly weights
      rXY' = rXY + sXY
  r <- lift rnd
  pure $ 0 === maybe 0 (`eval` r) (getZeroCoeff rXY')

-- | Constant term of t(X, Y) is zero, thus
-- demonstrating that the constraint system is satisfied
prop_tPoly_zero_constant :: Property
prop_tPoly_zero_constant = QCM.monadicIO $ do
  (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
  zeroCoeff <- QCM.run $ findTPolyZeroCoeff acircuit assignment
  pure $ zeroCoeff === 0
  where
    findTPolyZeroCoeff :: MonadRandom m => ArithCircuit Fr -> Assignment Fr -> m Fr
    findTPolyZeroCoeff circuit@ArithCircuit{..} assignment = do
      let n = case head (wL weights) of
                Nothing -> panic "Empty weights"
                Just xs -> length xs
      let rXY = rPoly assignment
          sXY = sPoly weights
          kY = kPoly cs n
          tP = tPoly rXY sXY kY

      r <- rnd
      pure $ maybe 0 (`eval` r) (getZeroCoeff tP)
