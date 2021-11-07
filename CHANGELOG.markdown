0.6 [????.??.??]
----------------
* Allow building with GHC 9.2.1.
* The `Data.Bits.Extras.oneBits` function now matches the implementation of the
  `oneBits` function from `base`'s `Data.Bits` module. This is a breaking
  change since the constraint has been strengthened from `Bits` to
  `FiniteBits`. If you need to invoke `oneBits` on a data type that does not
  have a `FiniteBits` instance (e.g., `Integer`), use the newly added
  `unsafeOneBits` function instead.
* Generalize `log2` to all `Integral` types with `base` >= 4.15 (GHC >= 9.0).

0.5.3 [2021.02.17]
------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.5.2 [2019.05.02]
------------------
* Support building with `base-4.13` (GHC 8.8).
* Add a `MonadFail` instance for `Coding`.

0.5.1
-----
* Fix off-by-one error in `putUnaligned`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.5
---
* ghc 8 support
* Fixed doctests

0.4
---
* Embrace `FiniteBits` from GHC 7.8.3 now that a platform has shipped with it.

0.3.3
-----
* Fixed dependencies on old busted versions

0.3.2
-----
* Bug fix for `getBit`

0.3.1
-----
* Fixed dependency bounds

0.3
---
* Support for `bytes` 0.8

0.2.1
-----
* Claim to be Trustworthy

0.2
---
* Crippled to work with `bytes` 0.7, so we can work with `binary >= 0.6`.

0.1
---
* Repository initialized
