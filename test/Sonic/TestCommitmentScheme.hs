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

import Text.PrettyPrint.Leijen.Text as PP

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
      d <- QCM.run (getRandomR (5, 100))
      max <- QCM.run (getRandomR ((d `quot` 2), d))
      let acExample = arithCircuitExample1 x z
          ArithCircuit{..} = aceCircuit acExample
          assignment = aceAssignment acExample

      let srs = SRS.new d x alpha
          n = length . aL $ assignment
          fX = evalOnY y $ tPoly (rPoly assignment) (sPoly weights) (kPoly cs n)
          commitment = commitPoly srs max fX
          commitment' = commitPoly' srs max x fX
      traceShowM (d, max, "difference", d - max)
      traceShowM ("commitment equal: ", commitment == commitment')
      let opening = openPoly srs commitment z fX
          opening' = openPoly' srs commitment' x z fX
      traceShowM ("opening equal: ", opening == opening')

      QCM.assert $ pcV srs max commitment z opening

