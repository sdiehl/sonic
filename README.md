<p align="center">
  <a href="http://www.adjoint.io"><img src="https://www.adjoint.io/assets/img/adjoint-logo@2x.png" width="250"/></a>
</p>

[Sonic](https://eprint.iacr.org/2019/550.pdf) is a zk-SNARK protocol for general
arithmetic circuit satisfiability with universal and updatable Structured Reference String (SRS) that scales linearly in size.

Sonic requires a trusted setup for an SRS, but the SRS can be continually
strengthened. Moreover, Sonic only requires a single setup for all circuits.

Usage
-----

Sonic allows a prover to demonstrate knowledge of a hidden witness (a, b, c) for
a given constraint system. Sonic defines its constraint system with respect to
the two-variate polynomial equation used in
[Bulletproofs](https://eprint.iacr.org/2017/1066.pdf). In the Bulletproofs
polynomial equation, there is one polynomial that is determined by
the statement and a second that is determined by the constraints.


```haskell
import Bulletproofs.ArithmeticCircuit
import Crypto.Number.Generate (generateMax, generateBetween)

--  Example:
--  5 linear constraints (q = 5):
--  aO[0] = aO[1]
--  aL[0] = V[0] - z
--  aL[1] = V[2] - z
--  aR[0] = V[1] - z
--  aR[1] = V[3] - z
--
--  2 multiplication constraint (implicit) (n = 2):
--  aL[0] * aR[0] = aO[0]
--  aL[1] * aR[1] = aO[1]
--
--  4 input values (m = 4)

sonicProtocol :: ArithCircuit f -> Assignment f -> SRS -> f -> IO Bool
sonicProtocol
  circuit@(ArithCircuit gates wV cs) assignment cs
  assignment@(Assignment aL aR aO)
  srs 
  x = do
    (proof, y, z, ys) <- prover srs gateInputs arithCircuit x
    pure $ verifier srs arithCircuit proof y z ys

runExample :: IO Bool
runExample = do
  -- SRS
  x <- Fr.random
  alpha <- Fr.random
  d <- generateBetween 2 100
  let srs = SRS.new d x alpha

  -- Arithmetic circuit
  cs = [0, -z, -z, -z, -z]
  wV = [[0, 0, 0, 0]
       ,[1, 0, 0, 0]
       ,[0, 0, 1, 0]
       ,[0, 1, 0 ,0]
       ,[0, 0, 0, 1]]
  arithCircuit = ArithCircuit gateWeights wV cs

  -- Assignment
  z <- Fr.random
  aL = [4 - z, 9 - z]
  aR = [9 - z, 4 - z]
  aO = aL `hadamardp` aR
  assignment = Assignment aL aR aO

  -- Run protocol
  sonicProtocol arithCircuit assignment srs x

  where
    gateWeights :: GateWeights Fr
    gateWeights = GateWeights
    { wL = [[0, 0]
           ,[1, 0]
           ,[0, 1]
           ,[0, 0]
           ,[0, 0]]
    , wR = [[0, 0]
	   ,[0, 0]
           ,[0, 0]
           ,[1, 0]
           ,[0, 1]]
    , wO = [[1, -1]
           ,[0, 0]
           ,[0, 0]
           ,[0, 0]
           ,[0, 0]]
    }
```

References
----------

1.  Maller M., Bowe S., Kohlweiss M. and Meiklejohn S.
    "Sonic: Zero-Knowledge SNARKs from Linear-Size Universal and Updateable
    Structured Reference Strings"
	(https://eprint.iacr.org/2019/099)


2.  Bunz B., Bootle J., Boneh D., Poelstra A., Wuille P., Maxwell G.
    "Bulletproofs: Short Proofs for Confidential Transactions and More".
	(https://eprint.iacr.org/2017/1066.pdf)
