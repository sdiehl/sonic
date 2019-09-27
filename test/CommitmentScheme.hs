{-# LANGUAGE RecordWildCards #-}
module CommitmentScheme where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (rnd)
import Math.Polynomial.Laurent
import Bulletproofs.ArithmeticCircuit

import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Utils
import qualified Sonic.SRS as SRS
import Data.Pairing.BLS12381
import Reference

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
          fX = evalOnY pY $ tPoly (rPoly assignment) (sPoly weights) (kPoly cs n)
          commitment = commitPoly srs d fX
          opening = openPoly srs commitment pZ fX

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
      case flip evalLaurent r <$> getZeroCoeff tP of
        Nothing -> panic "Zero coeff does not exist"
        Just z -> pure z

-- R ← Commit(bp,srs,n,r(X,1))
-- (a=r(z,1),Wa) ← Open(R,z,r(X,1))
-- check pcV(bp,srs,n,R,z,(a,Wa))
test_rX1_commit_scheme :: TestTree
test_rX1_commit_scheme = localOption (QuickCheckTests 25) $
  testProperty "rX1 commitment scheme" $ QCM.monadicIO $ do
      RandomParams{..} <- lift randomParams
      (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
      let n = length . aL $ assignment
      d <- lift $ randomD n

      let srs = SRS.new d pX pAlpha
      cns <- lift $ replicateM 4 rnd
      let rXY = rPoly assignment
          sumcXY = newLaurent
                   (negate (2 * n + 4))
                   (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
          polyR' = rXY + sumcXY
          commitment = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')
          opening = openPoly srs commitment pZ (evalOnY 1 polyR')

      QCM.assert $ pcV srs n commitment pZ opening

-- R ← Commit(bp,srs,n,r(X,1))
-- (a=r(z,1),Wa) ← Open(R,yz,r(X,1))
-- check pcV(bp,srs,n,R,yz,(a,Wa))
test_rX1YZ_commit_scheme :: TestTree
test_rX1YZ_commit_scheme = localOption (QuickCheckTests 25) $
  testProperty "rX1YZ commitment scheme" $ QCM.monadicIO $ do
      RandomParams{..} <- lift randomParams

      (acircuit@ArithCircuit{..}, assignment) <- lift . generate $ rndCircuit
      let n = length . aL $ assignment

      d <- lift $ randomD n

      let srs = SRS.new d pX pAlpha
      cns <- lift $ replicateM 4 rnd
      let rXY = rPoly assignment
          sumcXY = newLaurent
                   (negate (2 * n + 4))
                   (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
          polyR' = rXY + sumcXY
          commitment = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')
          opening = openPoly srs commitment (pY * pZ) (evalOnY 1 polyR')

      QCM.assert $ pcV srs n commitment (pY * pZ) opening
