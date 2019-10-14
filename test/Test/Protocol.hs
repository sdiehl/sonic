{-# LANGUAGE RecordWildCards #-}
module Test.Protocol where

import Protolude
import Bulletproofs.ArithmeticCircuit
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM

import Sonic.Protocol
import qualified Sonic.SRS as SRS
import Test.Reference

test_sonic :: TestTree
test_sonic = localOption (QuickCheckTests 50)
  $ testProperty "Sonic protocol" $ QCM.monadicIO $ do
    RandomParams {..} <- lift randomParams
    (arithCircuit, assignment@Assignment{..}) <- lift . generate $ rndCircuit
    let n = length aL
    d <- lift $ randomD n
    let srs = SRS.new d pX pAlpha
    (proof, rndOracle@RndOracle{..}) <- lift $ prove srs assignment arithCircuit
    QCM.assert $ verify srs arithCircuit proof rndOracleY rndOracleZ rndOracleYZs
