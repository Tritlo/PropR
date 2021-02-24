[![Build Status](https://www.travis-ci.com/Tritlo/ghc-synth.svg?token=YpmPqezsnvxxwFc8zMFo&branch=master)](https://www.travis-ci.com/Tritlo/ghc-synth)

Synthesis using GHC
===================

Works, but is pretty slow at the moment, even when doing the synthesis and
testing in parallel, but it works! Requires QuickCheck to be installed globally,
(or in someway such that it is picked up by the GHC API, from the libdir I guess)
otherwise the internal synthesizer cannot run the tests (we should fix this
by some NIX magic).

The synthesis part itself is quite fast... it's the filtering of the search
space that's slow, since we are essentially compiling every expression and
booting the whole GHC each time... but we have to do it in a new process due
to potentially infinite expressions (which cannot be timedout if they are
non-yielding, unless we recompile the base libraries).

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

Results
-------
When synthesizing for `(2,2,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",["propIsSymmetric f xs = f xs == f (reverse xs)"])` the runtime (without debug) was

```
real	4m34.566s
user	0m50.918s
sys	    3m37.273s
```

Finding `5843` matches, including `(foldl (+) zero)` and
weirder ones (which are indeed symmetric, the only property we asked for) like
`(foldr (curry product) zero)`.

The full output (with debug) be seen in `out-2holes-depth2.txt`.


More interesting for 2 holes and depth 1 (i.e. we recursively add holes once)


```
[nix-shell:~/ghc-synth]$ time cabal run
Up to date
SCOPE:
  import Prelude hiding (id, ($), ($!), asTypeOf)
TARGET TYPE:
  [a] -> Int
MUST SATISFY:
  prop_is_symmetric f xs = f xs == f (reverse xs)
  prop_bin f = f [] == 0 || f [] == 1
  prop_not_const f x = not ((f []) == f [x])
IN CONTEXT:
  zero = 0 :: Int
  one = 1 :: Int
PARAMETERS:
  MAX HOLES: 2
  MAX DEPTH: 1
SYNTHESIZING...
GENERATING CANDIDATES...DONE!
GENERATED 41 CANDIDATES!
COMPILING CANDIDATE CHECKS...DONE!
CHECKING 41 CANDIDATES...DONE!
FOUND MATCH:
length

real	0m2.966s
user	0m1.768s
sys		0m1.376s

```

