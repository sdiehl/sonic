{-# LANGUAGE RecordWildCards #-}
module Test.CommitmentScheme where

import Protolude

import Bulletproofs.ArithmeticCircuit
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (rnd)
import Data.Pairing.BLS12381
import Data.Poly.Sparse.Laurent (eval, monomial)
import qualified GHC.Exts
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale)
import qualified Test.QuickCheck.Monadic as QCM

import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Utils
import qualified Sonic.SRS as SRS
import Test.Reference

-- T ← Commit(bp,srs,d,t(X,y))
-- (t=t(z,y),Wt) ← Open(T,z,t(X,y)))
-- check pcV(bp,srs,d,T,z,(t,Wt))
test_tXy_commit_scheme :: TestTree
test_tXy_commit_scheme = localOption (QuickCheckTests 25) $
  testProperty "tXy commitment scheme" $ QCM.monadicIO $ do
      RandomParams{..} <- lift randomParams
      (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
      let n = length . aL $ assignment
      d <- lift $ randomD n
      zeroCoeff <- lift $ findZeroCoeff acircuit assignment
      QCM.assert $ zeroCoeff == 0

      let srs = SRS.new d pX pAlpha
          fX = evalY pY $ tPoly (rPoly assignment) (sPoly weights) (kPoly cs n)
          commitment = commitPoly srs d fX
          opening = openPoly srs pZ fX

      QCM.assert $ pcV srs d commitment pZ opening
  where
    findZeroCoeff :: MonadRandom m => ArithCircuit Fr -> Assignment Fr -> m Fr
    findZeroCoeff circuit@ArithCircuit{..} assignment = do
      let n = case head (wL weights) of
                Nothing -> panic "Empty weights"
                Just xs -> length xs
      let rXY = rPoly assignment
          sXY = sPoly weights
          kY = kPoly cs n
          tP = tPoly rXY sXY kY

      r <- rnd
      pure $ maybe 0 (flip eval r) (getZeroCoeff tP)

-- R ← Commit(bp,srs,n,r(X,1))
-- (a=r(z,1),Wa) ← Open(R,z,r(X,1))
-- check pcV(bp,srs,n,R,z,(a,Wa))
test_rX1_commit_scheme :: TestTree
test_rX1_commit_scheme = localOption (QuickCheckTests 50) $
  testProperty "rX1 commitment scheme" $ QCM.monadicIO $ do
      RandomParams{..} <- lift randomParams
      (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
      let n = length . aL $ assignment
      d <- lift $ randomD n

      let srs = SRS.new d pX pAlpha
          rXY = rPoly assignment
          commitment = commitPoly srs (fromIntegral n) (evalY 1 rXY)
          opening = openPoly srs pZ (evalY 1 rXY)

      QCM.assert $ pcV srs n commitment pZ opening

-- R ← Commit(bp,srs,n,r(X,1))
-- (a=r(z,1),Wa) ← Open(R,yz,r(X,1))
-- check pcV(bp,srs,n,R,yz,(a,Wa))
test_rX1YZ_commit_scheme :: TestTree
test_rX1YZ_commit_scheme = localOption (QuickCheckTests 50) $
  testProperty "rX1YZ commitment scheme" $ QCM.monadicIO $ do
      RandomParams{..} <- lift randomParams

      (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
      let n = length . aL $ assignment

      d <- lift $ randomD n

      let srs = SRS.new d pX pAlpha
      cns <- lift $ replicateM 4 rnd
      let rXY = rPoly assignment
          sumcXY :: BiVLaurent Fr
          sumcXY = GHC.Exts.fromList $
            zipWith (\i cni -> (negate (2 * n + i), monomial (negate (2 * n + i)) cni)) [1..] cns
          polyR' = rXY + sumcXY
          commitment = commitPoly srs (fromIntegral n) (evalY 1 polyR')
          opening = openPoly srs (pY * pZ) (evalY 1 polyR')

      QCM.assert $ pcV srs n commitment (pY * pZ) opening
