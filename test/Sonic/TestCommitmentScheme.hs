{-# LANGUAGE RecordWildCards #-}
module Sonic.TestCommitmentScheme where

import Protolude
import Pairing.Fr as Fr (Fr(..), new, random)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Crypto.Number.Generate (generateMax, generateBetween)

import Bulletproofs.ArithmeticCircuit
import Sonic.Constraints
import Sonic.CommitmentScheme
import Sonic.Utils
import qualified Sonic.SRS as SRS

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
      x <- QCM.run Fr.random
      y <- QCM.run Fr.random
      z <- QCM.run Fr.random
      alpha <- QCM.run Fr.random
      d <- QCM.run (generateBetween 2 100)
      max <- QCM.run (generateBetween (d `quot` 2) (2*d-1))

      let bL0 = 7
          bR0 = 3
          bL1 = 2

      let wL = [[1]
               ,[0]]
          wR = [[0]
               ,[1]]
          wO = [[0]
               ,[0]]
          cs = [bL0 + bR0, bL1 + 10]
          aL = [10]
          aR = [12]
          aO = aL `hadamardp` aR
          gateWeights = GateWeights wL wR wO
          gateInputs = Assignment aL aR aO
          arithCircuit = ArithCircuit gateWeights [] cs

      let srs = SRS.new d x alpha
          n = length aL
          fX = evalOnY y $ tPoly (rPoly gateInputs) (sPoly gateWeights) (polyK cs n)
          commitment = commitPoly srs max fX
          opening = openPoly srs commitment z fX

      QCM.assert $ pcV srs max commitment z opening

