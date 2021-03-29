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

to see it in action on the `foldl (-) 0` example. This produces the following:

```
TARGET:
  `broken` in tests/BrokenModule.hs
SCOPE:
  import Prelude hiding (id, ($), ($!), asTypeOf)
TARGET TYPE:
  [Int] -> Int
MUST SATISFY:
  prop'_isSum broken xs = broken xs == sum xs
IN CONTEXT:
  prop_isSum :: [Int] -> Bool
  prop_isSum xs = broken xs == sum xs
  broken :: [Int] -> Int
  broken = foldl (-) 0
  add :: Int -> Int -> Int
  add = (+)
  main :: IO ()
  main = print "Unrelated main function"
PARAMETERS:
  MAX HOLES: 2
  MAX DEPTH: 1
PROGRAM TO REPAIR: 
let
  broken :: [Int] -> Int
  broken = foldl (-) 0
in broken
FAILING PROPS:
  prop'_isSum broken xs = broken xs == sum xs
COUNTER EXAMPLES:
  [1]
TRACE OF COUNTER EXAMPLES:
  (RealSrcSpan SrcSpanMultiLine "FakeTarget38652-0.hs" 0 -1 4 10,Nothing,[(TopLevelBox ["fake_target"],1)],1)
  (RealSrcSpan SrcSpanMultiLine "FakeTarget38652-0.hs" 1 1 4 10,Just "let\n  broken :: [Int] -> Int\n  broken = foldl (-) 0\nin broken",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget38652-0.hs" 3 3 23,Nothing,[(LocalBox ["fake_target","broken"],1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget38652-0.hs" 3 12 23,Just "foldl (-) 0",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget38652-0.hs" 3 18 21,Just "(-)",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget38652-0.hs" 3 22 23,Just "0",[(ExpBox False,1)],1)

REPAIRING...DONE! (6.03s)
REPAIRS:
  tests/BrokenModule.hs:8:1-20
  -broken = foldl (-) 0
  +broken = sum

  tests/BrokenModule.hs:8:1-20
  -broken = foldl (-) 0
  +broken = foldl add 0

  tests/BrokenModule.hs:8:1-20
  -broken = foldl (-) 0
  +broken = foldl (+) 0

SYNTHESIZING...
GENERATING CANDIDATES...DONE!
GENERATED 130 CANDIDATES!
COMPILING CANDIDATE CHECKS...DONE!
CHECKING 130 CANDIDATES...DONE!
DONE! (4.88s)
FOUND MATCH:
sum
```

Showing how it works.

For the `BrokenGCD` module, we get an interesting result:

```
$ cabal run ghc-synth -- tests/BrokenGCD.hs
```

gives us:

```
REPAIRS:
  tests/BrokenGCD.hs:(17,1)-(21,28)
  -gcd' 0 b = gcd' 0 b
  +gcd' 0 b = b
  gcd' a b | b == 0 = a
  gcd' a b = if (a > b) then gcd' (a - b) b else gcd' a (b - a)
```

Showing how we could fix the infinitely looping `gcd` program.

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
