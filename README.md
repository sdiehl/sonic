<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

[Sonic](https://eprint.iacr.org/2019/099.pdf) [1] is a zk-SNARK protocol for general
arithmetic circuit satisfiability with universal and updatable Structured
Reference String (SRS) proposed by [Groth et
al.](https://eprint.iacr.org/2018/280.pdf) [2] that scales linearly in size.
Despite requiring a trusted setup for an SRS, the SRS can be continually
strengthened and Sonic only requires a single setup for all circuits.

Sonic allows a prover to demonstrate knowledge of a hidden witness for
a given constraint system. It defines its constraint system with respect to
the two-variate polynomial equation used in
[Bulletproofs](https://eprint.iacr.org/2017/1066.pdf) [3].

### Usage

The Sonic protocol can be outlined in three steps: Setup, Prover and
Verifier. Due to the universality property of the SRS, the setup phase needs
only to be run once. This implementation uses BLS12-381 elliptic curve.

```haskell
sonicProtocol :: ArithCircuit Fr -> Assignment Fr -> Fr -> IO Bool
sonicProtocol circuit assignment x = do
  -- Setup for an SRS
  srs <- SRS.new <$> randomD n <*> pure x <*> rnd
  -- Prover
  (proof, y, z, ys) <- prove srs assignment circuit
  -- Verifier
  pure $ verify srs circuit proof y z ys
  where
    -- Number of multiplication constraints
    n = length $ aL assignment
    -- Note that 'd' should be large enough to support the circuit depth 'n'
    randomD n = getRandomR (7 * n, 100 * n)
```

The following example takes an arithmetic circuit of 5 linear constraints and 2
multiplication constraints:

```haskell
runExample :: IO ()
runExample = do
  pX <- rnd
  pZ <- rnd
  let (arithCircuit, assignment@Assignment{..}) = arithCircuitExample pX pZ
  success <- sonicProtocol arithCircuit assignment pX
  putText $ "Success: " <> show success
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
