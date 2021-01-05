Synthesis using GHC
===================

Works, but is pretty slow at the moment, even when doing the synthesis and
testing in parallel, but it works! Requires QuickCheck to be installed globally,
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
When synthesizing for `(2,2,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",["propIsSymmetric f xs = f xs == f (reverse xs)"])` the runtime was

```
real	61m5.095s
user	44m22.003s
sys	16m28.323s
```

Finding `5844` matches, including `(foldl (+) zero)` and
weirder ones (which are indeed symmetric, the only property we asked for) like
`(foldr (curry product) (id zero))`.

The full output can be seen in `out-2holes-depth3.txt`, but note that there the
depth is one higher than it should be (fixed now for consistency)



More interesting for 2 holes and depth 1 (i.e. we recursively add holes once)

```
[nix-shell:~/ghc-synth]$ time cabal run ghc-synth -- -fholes=2 -fdepth=1
Up to date
SCOPE:
  import Prelude hiding (id, ($), ($!), asTypeOf)
  import Test.QuickCheck (quickCheckWithResult, Result(..), stdArgs, Args(..), isSuccess, (==>))
TARGET TYPE:
  [Int] -> Int
MUST SATISFY:
  prop_IsSymmetric f xs = f xs == f (reverse xs)
  prop_Bin f = f [] == 0 || f [] == 1
  prop_not_const f = not ((f []) == f [1,2,3])
IN CONTEXT:
  zero = 0 :: Int
  one = 1 :: Int
PARAMETERS:
  MAX HOLES: 2
  MAX DEPTH: 1
SYNTHESIZING...
CHECKING 201 CANDIDATES...
FOUND 19 MATCHES:
length
product
sum
(foldl (-) zero)
(foldl (-) one)
(foldl gcd zero)
(foldl (*) one)
(foldl (+) zero)
(foldl (+) one)
(foldl max zero)
(foldl max one)
(foldr subtract zero)
(foldr subtract one)
(foldr gcd zero)
(foldr (*) one)
(foldr (+) zero)
(foldr (+) one)
(foldr max zero)
(foldr max one)

real	0m39.725s
user	0m29.947s
sys		0m9.876s
```

