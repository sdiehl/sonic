{-# LANGUAGE RecordWildCards #-}
module Protocol where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Monadic as QCM
import Bulletproofs.ArithmeticCircuit

import Sonic.Protocol
import qualified Sonic.SRS as SRS
import Reference

test_sonic :: TestTree
test_sonic = localOption (QuickCheckTests 25)
  $ testProperty "Sonic protocol" $ QCM.monadicIO $ do
    RandomParams {..} <- lift randomParams
    (arithCircuit, assignment@Assignment{..}) <- lift . generate $ rndCircuit
    let n = length aL
    d <- lift $ randomD n
    let srs = SRS.new d pX pAlpha
    (proof, y, z, ys) <- lift $ prove srs assignment arithCircuit
    QCM.assert $ verify srs arithCircuit proof y z ys
