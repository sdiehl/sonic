module Main where

import Protolude
import Bulletproofs.ArithmeticCircuit (Assignment(..), ArithCircuit)
import Criterion.Main
import Data.Pairing.BLS12381 (Fr)

import qualified Sonic.SRS as SRS
import Sonic.Protocol
import Test.Reference

exampleX :: Fr
exampleX = 11

exampleZ :: Fr
exampleZ = 12

exampleD :: Int -> Int
exampleD n = 25 * n

exampleRndParams :: RandomParams
exampleRndParams = RandomParams
  { pX = 1
  , pY = 2
  , pZ = 3
  , pAlpha = 4
  }

runProver :: IO (Proof, RndOracle)
runProver = do
  let (arithCircuit, assignment) = arithCircuitExample1 exampleX exampleZ
      srs = SRS.new (exampleD (length $ aL assignment)) (pX exampleRndParams) (pAlpha exampleRndParams)
  prove srs assignment arithCircuit

main :: IO ()
main = defaultMain
  [ sonic $ arithCircuitExample1 exampleX exampleZ
  , sonic $ arithCircuitExample2 exampleX exampleZ
  ]

sonic :: (ArithCircuit Fr, Assignment Fr) -> Benchmark
sonic (arithCircuit, assignment) = bgroup "Sonic"
  [ bench "Prover" $
    let srs = SRS.new (exampleD (length $ aL assignment)) (pX exampleRndParams) (pAlpha exampleRndParams)
    in nfIO (prove srs assignment arithCircuit)
  , env runProver $ \(~(proof, rndOracle@RndOracle{..})) ->
      bench "Verifier" $
      let srs = SRS.new (exampleD (length $ aL assignment)) (pX exampleRndParams) (pAlpha exampleRndParams)
      in nf (verify srs arithCircuit proof rndOracleY rndOracleZ) rndOracleYZs
  ]
