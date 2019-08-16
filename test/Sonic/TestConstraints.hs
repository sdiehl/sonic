{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Sonic.TestConstraints where

import Protolude
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Test.QuickCheck.Monadic as QCM
import Data.List (zipWith4)
import Control.Monad.Random (MonadRandom, getRandomR)

import GaloisField(GaloisField(rnd))
import Bulletproofs.ArithmeticCircuit
import Bulletproofs.ArithmeticCircuit.Internal (computeInputValues, arithAssignmentGen)
import Bulletproofs.Utils (commit)
import Math.Polynomial.Laurent
import Text.PrettyPrint.Leijen.Text as PP hiding ((<$>), dot)

import Sonic.Utils
import Sonic.Constraints
import Sonic.Reference
import Sonic.Curve (Fr, Fq)

----------------
-- Unit tests --
----------------

rPolyInput :: Assignment Fr
rPolyInput = Assignment
  { aL = [1, 2]
  , aR = [3, 4]
  , aO = [5, 6]
  }

rPolyOutput :: BiVariateLaurent Fr
rPolyOutput = newLaurent (-4)
  [ Laurent (-4) [6]
  , Laurent (-3) [5]
  , Laurent (-2) [4]
  , Laurent (-1) [3]
  , Laurent 0 []
  , Laurent 1 [1]
  , Laurent 2 [2]
  ]


unit_rPoly_unit :: Assertion
unit_rPoly_unit = assertBool "Incorrect rPoly"
  (rPoly rPolyInput == rPolyOutput)

sPolyInput :: GateWeights Fr
sPolyInput = GateWeights
  { wL = [[1, 2], [3, 4]]
  , wR = [[5, 6], [7, 8]]
  , wO = [[9, 10], [11, 12]]
  }

sPolyOutput :: BiVariateLaurent Fr
sPolyOutput = newLaurent (-2)
  [ newLaurent 0 [0, 0, 0, 3, 4]
  , newLaurent 0 [0, 0, 0, 1, 2]
  , newLaurent 0 []
  , newLaurent 0 [0, 0, 0, 5, 6]
  , newLaurent 0 [0, 0, 0, 7, 8]
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

unit_sPoly_unit :: Assertion
unit_sPoly_unit = assertBool "Incorrect sPoly"
  (sPoly sPolyInput == sPolyOutput)

--------------------
-- Property tests --
--------------------

-- | r(X, Y) = r(XY, 1)
prop_rPoly_prop :: Fr -> Fr -> Property
prop_rPoly_prop x y = QCM.monadicIO $ do
  assignment <- lift $ generate $ arithAssignmentGen 3
  let rP = rPoly assignment
  pure $ evalLaurent (evalOnY y rP) x === evalLaurent (evalOnY 1 rP) (x * y)

-- | Constant term in polynomial R[x] is zero
prop_rPoly_zero_constant :: Fr -> Fr -> Property
prop_rPoly_zero_constant x y = QCM.monadicIO $ do
  aL <- QCM.run $ replicateM 10 rnd
  aR <- QCM.run $ replicateM 10 rnd
  let aO = aL `hadamardp` aR
      rP = rPoly @Fr (Assignment aL aR aO)
      constantElemX = take 1 $ drop (abs (expLaurent rP)) $ coeffsLaurent rP
  pure $ case constantElemX of
    [] -> True === True
    [constantElemXL] ->
      let constantElemY = take 1 $ drop (abs (expLaurent rP)) $ coeffsLaurent constantElemXL
      in (True === null constantElemY)

-- | Constant term of t(X, Y) is zero, thus
-- demonstrating that the constraint system is satisfied
prop_tPoly_zero_constant :: Property
prop_tPoly_zero_constant = QCM.monadicIO $ do
  x <- QCM.run rnd
  z <- QCM.run rnd
  let acExample@ACExample{..} = arithCircuitExample3 x z
  zeroCoeff <- QCM.run $ findZeroCoeff aceCircuit aceAssignment
  pure $ zeroCoeff === 0
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

      case getZeroCoeff tP >>= getZeroCoeff of
        Nothing -> panic "Zero coeff does not exist"
        Just z -> pure z


