<p align="center">
  <a href="http://www.adjoint.io"><img src="https://www.adjoint.io/assets/img/adjoint-logo@2x.png" width="250"/></a>
</p>

[Sonic](https://eprint.iacr.org/2019/550.pdf) [1] is a zk-SNARK protocol for general
arithmetic circuit satisfiability with universal and updatable Structured
Reference String (SRS) proposed by [Groth et
al.](https://eprint.iacr.org/2018/280.pdf) [2] that scales linearly in size.
Despite requiring a trusted setup for an SRS, the SRS can be continually
strengthened and Sonic only requires a single setup for all circuits.

Sonic allows a prover to demonstrate knowledge of a hidden witness for
a given constraint system. It defines its constraint system with respect to
the two-variate polynomial equation used in
[Bulletproofs](https://eprint.iacr.org/2017/1066.pdf) [3]. In the Bulletproofs
polynomial equation, there is one polynomial that is determined by
the statement and a second that is determined by the constraints.

### Usage

The witness and the constraint system are defined as follows:

```haskell
data ArithCircuit f
  = ArithCircuit
    { weights :: GateWeights f
      -- ^ Weights for vectors of left and right inputs and for vector of outputs
    , commitmentWeights :: [[f]]
      -- ^ Weigths for a commitments V of rank m
    , cs :: [f]
      -- ^ Vector of constants of size Q
    }

data GateWeights f
  = GateWeights
    { wL :: [[f]] -- ^ WL ∈ F^(Q x n)
    , wR :: [[f]] -- ^ WR ∈ F^(Q x n)
    , wO :: [[f]] -- ^ WO ∈ F^(Q x n)
    }

data Assignment f
  = Assignment
    { aL :: [f] -- ^ aL ∈ F^n. Vector of left inputs of each multiplication gate
    , aR :: [f] -- ^ aR ∈ F^n. Vector of right inputs of each multiplication gate
    , aO :: [f] -- ^ aO ∈ F^n. Vector of outputs of each multiplication gate
    }
```

The Sonic protocol can be outlined in three steps: Setup, Prover and
Verifier. Due to the universality property of the SRS, the setup phase needs
only to be run once. The `Fr` parameter used in the example above is defined in [Adjoint's pairing
library](https://github.com/adjoint-io/pairing), but the protocol works with other finite fields.

```haskell
sonicProtocol :: ArithCircuit Fr -> Assignment Fr -> Fr -> IO Bool
sonicProtocol circuit@(ArithCircuit gates wV cs) assignment x = do
  -- Setup for an SRS
  srs <- SRS.new <$> generateBetween 2 100 <*> pure x <*> Fr.random
  -- Prover
  (proof, y, z, ys) <- prover srs assignment circuit x
  -- Verifier
  pure $ verifier srs circuit proof y z ys
```

The following example of an arithmetic circuit uses 5 linear constraints and 2
multiplication constraints:

```haskell
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
runExample :: IO ()
runExample = do
  -- Arithmetic circuit
  z <- Fr.random
  let cs = [0, -z, -z, -z, -z]
      wV = [[0, 0, 0, 0]
           ,[1, 0, 0, 0]
           ,[0, 0, 1, 0]
           ,[0, 1, 0 ,0]
           ,[0, 0, 0, 1]]
      arithCircuit = ArithCircuit gateWeights wV cs

  -- Assignment
  let aL = [4 - z, 9 - z]
      aR = [9 - z, 4 - z]
      aO = aL `hadamardp` aR
      assignment = Assignment aL aR aO

  -- Run protocol
  print =<< sonicProtocol arithCircuit assignment =<< Fr.random

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

The complete code of the example above can be found [here](examples/Main.hs).

References
----------

1.  Maller M., Bowe S., Kohlweiss M. and Meiklejohn S.
    "Sonic: Zero-Knowledge SNARKs from Linear-Size Universal and Updateable
    Structured Reference Strings", 2019.
	https://eprint.iacr.org/2019/099

2. Groth J., Kohlweiss M., Maller M., Meiklejohn S., Miers M.
   "Updatable and Universal Common Reference Strings with Applications to
   zk-SNARKs", 2018.
   https://eprint.iacr.org/2018/280.pdf

3.  Bunz B., Bootle J., Boneh D., Poelstra A., Wuille P., Maxwell G.
    "Bulletproofs: Short Proofs for Confidential Transactions and More", 2018.
	https://eprint.iacr.org/2017/1066.pdf
