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
test_poly_commit_scheme :: TestTree
test_poly_commit_scheme
  = testProperty "Polynomial commitment scheme" $ QCM.monadicIO $ do
      x <- QCM.run rnd
      y <- QCM.run rnd
      z <- QCM.run rnd
      alpha <- QCM.run rnd

      let acExample = arithCircuitExample2 x z
          acircuit@ArithCircuit{..} = aceCircuit acExample
          assignment = aceAssignment acExample
          n = length . aL $ assignment

      d <- QCM.run (getRandomR (3 * n + 1, 20 * n))

      -- "...in our polynomial constraint system 3n < d
      -- (otherwisewe cannot commit to t(X,Y)),
      -- thus r(X,Y) has no (−d + n) term."
      max <- QCM.run . generate $ oneof
        [ pure d
        , arbitrary `suchThat` (\i -> i > 0 && 3 * i < d)
        ]
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

