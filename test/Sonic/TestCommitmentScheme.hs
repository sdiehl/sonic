{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Sonic.TestCommitmentScheme where

import Protolude

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Control.Monad.Random (getRandomR)
import GaloisField(GaloisField(rnd))
import Control.Monad.Random (MonadRandom, getRandomR)
import Math.Polynomial.Laurent
import Bulletproofs.ArithmeticCircuit
import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Utils
import qualified Sonic.SRS as SRS
import Sonic.Curve  (Fr)
import Sonic.Reference

-- Example of arithmetic circuit
--
-- bL0     bR0    bL1      10
--  |       |      |       |
--  |--[+]--|      |--[+]--|
--      |              |
--      | bO0      bO1 |
--      |  =        =  |
--      |  aL      aR  |
--      |-----[x]------|
--             |
--             | aO
--             |
test_tXy_commit_scheme :: TestTree
test_tXy_commit_scheme = localOption (QuickCheckTests 50) $
  testProperty "tXy commitment scheme" $ QCM.monadicIO $ do
      x <- QCM.run rnd
      y <- QCM.run rnd
      z <- QCM.run rnd
      alpha <- QCM.run rnd

      let acExample = arithCircuitExample2 x z
          acircuit@ArithCircuit{..} = aceCircuit acExample
          assignment = aceAssignment acExample
          n = length . aL $ assignment

      -- "...in our polynomial constraint system 3n < d
      -- (otherwisewe cannot commit to t(X,Y)),
      -- thus r(X,Y) has no (−d + n) term."
      d <- QCM.run (getRandomR (4 * n, 20 * n))
      let max = d
      zeroCoeff <- QCM.run $ findZeroCoeff acircuit assignment

      QCM.assert $ zeroCoeff == 0

      let srs = SRS.new d x alpha
          fX = evalOnY y $ tPoly (rPoly assignment) (sPoly weights) (kPoly cs n)
          commitment = commitPoly srs max fX
          opening = openPoly srs commitment z fX

      QCM.assert $ pcV srs max commitment z opening
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

test_rX1_commit_scheme :: TestTree
test_rX1_commit_scheme = localOption (QuickCheckTests 50) $
  testProperty "rX1 commitment scheme" $ QCM.monadicIO $ do
      x <- QCM.run rnd
      z <- QCM.run rnd
      alpha <- QCM.run rnd

      let acExample = arithCircuitExample2 x z
          acircuit@ArithCircuit{..} = aceCircuit acExample
          assignment = aceAssignment acExample
          n = length . aL $ assignment

      -- "...in our polynomial constraint system 3n < d
      -- (otherwisewe cannot commit to t(X,Y)),
      -- thus r(X,Y) has no (−d + n) term."
      d <- QCM.run (getRandomR (4 * n, 20 * n))
      let max = n

      let srs = SRS.new d x alpha
      cns <- QCM.run $ replicateM 4 rnd
      let rXY = rPoly assignment
          sumcXY = newLaurent
                   (negate (2 * n + 4))
                   (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
          polyR' = addLaurent rXY sumcXY
          commitment = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')
          opening = openPoly srs commitment z (evalOnY 1 polyR')

      QCM.assert $ pcV srs max commitment z opening

test_rX1YZ_commit_scheme :: TestTree
test_rX1YZ_commit_scheme = localOption (QuickCheckTests 50) $
  testProperty "rX1YZ commitment scheme" $ QCM.monadicIO $ do
      x <- QCM.run rnd
      y <- QCM.run rnd
      z <- QCM.run rnd
      alpha <- QCM.run rnd

      let acExample = arithCircuitExample2 x z
          acircuit@ArithCircuit{..} = aceCircuit acExample
          assignment = aceAssignment acExample
          n = length . aL $ assignment

      -- "...in our polynomial constraint system 3n < d
      -- (otherwisewe cannot commit to t(X,Y)),
      -- thus r(X,Y) has no (−d + n) term."
      d <- QCM.run (getRandomR (4 * n, 20 * n))
      let max = n

      let srs = SRS.new d x alpha
      cns <- QCM.run $ replicateM 4 rnd
      let rXY = rPoly assignment
          sumcXY = newLaurent
                   (negate (2 * n + 4))
                   (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
          polyR' = addLaurent rXY sumcXY
          commitment = commitPoly srs (fromIntegral n) (evalOnY 1 polyR')
          opening = openPoly srs commitment (y * z) (evalOnY 1 polyR')

      QCM.assert $ pcV srs max commitment (y * z) opening
