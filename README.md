[![Build Status](https://www.travis-ci.com/Tritlo/ghc-synth.svg?token=YpmPqezsnvxxwFc8zMFo&branch=master)](https://www.travis-ci.com/Tritlo/ghc-synth)
[![Haskell CI](https://github.com/Tritlo/ghc-synth/actions/workflows/haskell.yml/badge.svg)](https://github.com/Tritlo/ghc-synth/actions/workflows/haskell.yml)

Program repair using GHC
===================

Works, but is pretty slow at the moment, even when doing the synthesis and
testing in parallel, but it works! Requires QuickCheck to be installed globally,
(or in someway such that it is picked up by the GHC API, from the libdir I guess)
otherwise the internal synthesizer cannot run the tests.

The synthesis part itself is quite fast... it's the filtering of the search
space that's slow, since we are essentially compiling every expression and
booting the whole GHC each time... but we have to do it in a new process due
to potentially infinite expressions (which cannot be timedout if they are
non-yielding, unless we recompile the base libraries).


Usage
-----
This program requires version `3.4` or higher of `cabal`, and version `8.10` or
higher of `ghc`. A complete environment required to run (minus the `QuickCheck`)
is defined in `shell.nix`, and can be activated using `nix-shell`, if installed.

To run the program, ensure that `QuickCheck` is installed by running
`$ cabal install --lib QuickCheck `. You can then run

```
$ cabal run ghc-synth -- tests/BrokenModule.hs
```

to see it in action on the `foldl (-) 0` example, or you can run

```
$ cabal run ghc-synth -- tests/BrokenGCD.hs
```

to run it on the infinitely looping `gcd'`.

To run the tests, you can either run `cabal run test` or `cabal test`. Note
that there is an up-to-date run of the tests over at Travis-CI, which can be
seen at the top of this document.

To try it out for different scenarios, feel free to change the `Broken` modules
in `tests/`, but note that AST coverage is pretty limited at the moment.

Parameters
---------

The synthesizer accepts a command line argument, `-fdebug`, which makes it
output A LOT more.

The `-fholes` parameter is 2 by default, and denotes the maximum number of
holes added to an expression to see if it can fit. Corresponds to the
`-frefinement-level-hole-fits` parameter for GHC.

The `-fdepth` is deep we go, i.e. `0` means that we only synthesize top-level
expressions, `1` we synthesize a top-level expression with holes in it, and then
fill those holes with top-level expressions, for `2` we allow holes in the
sub-expressions one level deep etc.
