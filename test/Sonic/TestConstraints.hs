{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Sonic.TestConstraints where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as QCM

-- We may want to classify these by curve
import Pairing.Fr as Fr
import Bulletproofs.Fq as Fq

import Bulletproofs.ArithmeticCircuit
import Bulletproofs.ArithmeticCircuit.Internal (computeInputValues)
import Bulletproofs.Utils (commit)
import Math.Polynomial.Laurent
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import PrimeField

import Sonic.Utils as Utils
import Sonic.Constraints

exampleAssignment :: KnownNat p => Assignment (PrimeField p)
exampleAssignment = Assignment
  { aL = [1, 2]
  , aR = [3, 4]
  , aO = [5, 6]
  }

exampleWeights :: KnownNat p => GateWeights (PrimeField p)
exampleWeights = GateWeights
    { wL = [[1, 2]
           ,[3, 4]]
    , wR = [[5, 6]
           ,[7, 8]]
    , wO = [[9, 10]
           ,[11, 12]]
    }

rPolyOutput :: KnownNat p => Laurent (Laurent (PrimeField p))
rPolyOutput = newLaurent (-4)
  [ newLaurent (-4) [6]
  , newLaurent (-3) [5]
  , newLaurent (-2) [4]
  , newLaurent (-1) [3]
  , newLaurent 0 []
  , newLaurent 1 [1]
  , newLaurent 2 [2]
  ]

sPolyOutput :: KnownNat p => Laurent (Laurent (PrimeField p))
sPolyOutput = newLaurent (-2)
    [ newLaurent 0
        [ 0
        , 0
        , 0
        , 3
        , 4
        ]
    , newLaurent 0
        [ 0
        , 0
        , 0
        , 1
        , 2
        ]
    , newLaurent 0 []
    , newLaurent 0
        [ 0
        , 0
        , 0
        , 5
        , 6
        ]
    , newLaurent 0
        [ 0
        , 0
        , 0
        , 7
        , 8
        ]
    , newLaurent (-1)
        [ 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , 0
        , 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , 0
        , 9
        , 10
        ]
    , newLaurent (-2)
        [ 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , 0
        , 0
        , 0
        , 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , 11
        , 12
        ]
    ]

assignmentTest :: KnownNat p => Assignment (PrimeField p)
assignmentTest
  = let aL = [10, 5, 3]
        aR = [12, 7, 2]
        aO = aL `hadamardp` aR
    in Assignment aL aR aO

unit_sPoly :: Assertion
unit_sPoly = assertBool "Incorrect sPoly"
  (sPoly @Fq exampleWeights == sPolyOutput)

unit_rPoly :: Assertion
unit_rPoly = assertBool "Incorrect rPoly"
  (rPoly @Fq exampleAssignment == rPolyOutput)

-- | Test that both variables in a bivariate polynomial have same exponents
prop_rPoly :: Fq -> Fq -> Property
prop_rPoly x y
  = (x /= 0 && y /= 0) ==>
    let rP = rPoly assignmentTest
    in evalLaurent (evalOnY y rP) x === evalLaurent (evalOnY 1 rP) (x * y)

-- | Test that the constant term in polynomial R[x] is zero
prop_rPoly_zero_constant :: Fq -> Fq -> Property
prop_rPoly_zero_constant x y
  = (x /= 0 && y /= 0) ==> QCM.monadicIO $ do
  aL <- QCM.run $ replicateM 10 Utils.random
  aR <- QCM.run $ replicateM 10 Utils.random
  let aO = aL `hadamardp` aR
      rP = rPoly @Fq (Assignment aL aR aO)
      constantElemX = take 1 $ drop (abs (expLaurent rP)) $ coeffsLaurent rP
  pure $ case constantElemX of
    [] -> True === True
    [constantElemXL] ->
      let constantElemY = take 1 $ drop (abs (expLaurent rP)) $ coeffsLaurent constantElemXL
      in (True === null constantElemY)

-- Test that the constant term in the t polynomial is not zero
-- prop_tPoly_not_zero_constant :: Fq -> Fq -> Property
-- prop_tPoly_not_zero_constant = QCM.monadicIO $ do
--   x <- QCM.run $ do
--     cns <- replicateM 4 Utils.random
--     let rXY = rPoly assignment
--         sumcXY = newLaurent
--                 (negate (2 * n + 4))
--                 (reverse $ zipWith (\cni i -> newLaurent (negate (2 * n + i)) [cni]) cns [1..])
--         polyR' = addLaurent rXY sumcXY
--     y <- Utils.random
--     let ky = kPoly cs n
--     let sP = sPoly weights
--     let tP = tPoly polyR' sP ky
--     pure x
--   traceShowM $ pretty x
--   pure $ True === True
