# Changelog for sonic

## 0.3

* Use BLS12-381 elliptic curve
* Add `RndOracle` data type as part of the proving step
* Add benchmarks

## 0.2

* Fix leak: Prover should not receive `x` and `g^{\alpha}` should not be shared.
* Update dependencies: galois-field, pairing, elliptic-curve and bulletproofs
* Test protocol with arbitrary circuits

## 0.1

* Initial release.
