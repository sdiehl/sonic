module Sonic.TestConstraints where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as QCM

import Pairing.Fr as Fr (Fr(..), new)
import Bulletproofs.ArithmeticCircuit
import Math.Polynomial.Laurent

import Sonic.Utils
import Sonic.Constraints

exampleAssignment :: Assignment Fr
exampleAssignment = Assignment
  { aL = [1, 2]
  , aR = [3, 4]
  , aO = [5, 6]
  }

exampleWeights :: GateWeights Fr
exampleWeights = GateWeights
    { wL = [[1, 2]
           ,[3, 4]]
    , wR = [[5, 6]
           ,[7, 8]]
    , wO = [[9, 10]
           ,[11, 12]]
    }

rPolyOutput :: Laurent (Laurent Fr)
rPolyOutput = newLaurent (-4)
  [ newLaurent (-4) [6]
  , newLaurent (-3) [5]
  , newLaurent (-2) [4]
  , newLaurent (-1) [3]
  , newLaurent 0 []
  , newLaurent 1 [1]
  , newLaurent 2 [2]
  ]

sPolyOutput :: Laurent (Laurent Fr)
sPolyOutput = newLaurent (-2)
    [ newLaurent 0
        [ Fr 0
        , Fr 0
        , Fr 0
        , Fr 3
        , Fr 4
        ]
    , newLaurent 0
        [ Fr 0
        , Fr 0
        , Fr 0
        , Fr 1
        , Fr 2
        ]
    , newLaurent 0 []
    , newLaurent 0
        [ Fr 0
        , Fr 0
        , Fr 0
        , Fr 5
        , Fr 6
        ]
    , newLaurent 0
        [ Fr 0
        , Fr 0
        , Fr 0
        , Fr 7
        , Fr 8
        ]
    , newLaurent (-1)
        [ Fr 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , Fr 0
        , Fr 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , Fr 0
        , Fr 9
        , Fr 10
        ]
    , newLaurent (-2)
        [ Fr 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , Fr 0
        , Fr 0
        , Fr 0
        , Fr 21888242871839275222246405745257275088548364400416034343698204186575808495616
        , Fr 11
        , Fr 12
        ]
    ]

unit_sPoly :: Assertion
unit_sPoly = assertBool "Incorrect sPoly"
  (sPoly exampleWeights == sPolyOutput)

unit_rPoly :: Assertion
unit_rPoly = assertBool "Incorrect rPoly"
  (rPoly exampleAssignment == rPolyOutput)


prop_rPoly :: Fr -> Fr -> Property
prop_rPoly x y
  = (x /= 0 && y /= 0) ==>
    let aL = [10, 5, 3]
        aR = [12, 7, 2]
        aO = aL `hadamardp` aR
        gateInputs = Assignment aL aR aO
        rP = rPoly gateInputs
    in evalLaurent (evalOnY y rP) x === evalLaurent (evalOnY 1 rP) (x * y)
