[![Haskell CI](https://github.com/Tritlo/Endemic/actions/workflows/haskell.yml/badge.svg)](https://github.com/Tritlo/Endemic/actions/workflows/haskell.yml)

Endemic: Genetic Program repair using GHC 🐣
===================

Inspired by GenProg.

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

**MacOS** users have to use `cabal configure --enable-optimization --enable-executable-dynamic`. Windows users might find some help [here](https://www.linux.org/pages/download/).

To run the program, ensure that `QuickCheck` is installed by running
`$ cabal install --lib QuickCheck `. You can then run 

```
$ cabal run endemic -- examples/BrokenModule.hs
```

For more options see #Usage and the `--help` flag.

to see it in action on the `foldl (-) 0` example. This produces the following:

```diff
TARGET:
  `broken` in examples/BrokenModule.hs
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
  (RealSrcSpan SrcSpanMultiLine "FakeTarget85802-0.hs" 0 -3 4 10,Nothing,[(TopLevelBox ["fake_target"],1)],1)
  (RealSrcSpan SrcSpanMultiLine "FakeTarget85802-0.hs" 1 1 4 10,Just "let\n  broken :: [Int] -> Int\n  broken = foldl (-) 0\nin broken",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget85802-0.hs" 3 3 23,Nothing,[(LocalBox ["fake_target","broken"],1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget85802-0.hs" 3 12 23,Just "foldl (-) 0",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget85802-0.hs" 3 18 21,Just "(-)",[(ExpBox False,1)],1)
  (RealSrcSpan SrcSpanOneLine "FakeTarget85802-0.hs" 3 22 23,Just "0",[(ExpBox False,1)],1)

REPAIRING...DONE! (2.43s)
REPAIRS:
diff --git a/examples/BrokenModule.hs b/examples/BrokenModule.hs
--- a/examples/BrokenModule.hs
+++ b/examples/BrokenModule.hs
@@ -8,1 +8,1 @@ broken = foldl (-) 0
-broken = foldl (-) 0
+broken = sum

diff --git a/examples/BrokenModule.hs b/examples/BrokenModule.hs
--- a/examples/BrokenModule.hs
+++ b/examples/BrokenModule.hs
@@ -8,1 +8,1 @@ broken = foldl (-) 0
-broken = foldl (-) 0
+broken = foldl add 0

diff --git a/examples/BrokenModule.hs b/examples/BrokenModule.hs
--- a/examples/BrokenModule.hs
+++ b/examples/BrokenModule.hs
@@ -8,1 +8,1 @@ broken = foldl (-) 0
-broken = foldl (-) 0
+broken = foldl (+) 0

SYNTHESIZING...
GENERATING CANDIDATES...DONE!
GENERATED 130 CANDIDATES!
COMPILING CANDIDATE CHECKS...DONE!
CHECKING 130 CANDIDATES...DONE!
DONE! (6.52s)
FOUND MATCH:
sum

```

Showing how it works.

For the `BrokenGCD` module, we get an interesting result:

```
$ cabal run endemic -- examples/BrokenGCD.hs
```

gives us:

```diff
REPAIRS:
diff --git a/examples/BrokenGCD.hs b/examples/BrokenGCD.hs
--- a/examples/BrokenGCD.hs
+++ b/examples/BrokenGCD.hs
@@ -17,3 +17,3 @@ gcd' 0 b = gcd' 0 b
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
in `examples/`, but note that AST coverage is pretty limited at the moment.

Usage
---------

```
endemic - Genetic program repair for Haskell

Usage: endemic [--log-loc] [--no-log-timestamp] [--log-level LOGLEVEL] 
               [--log-file FILE] [--seed INT] [--config CONFIG] 
               [--override CONFIG] TARGET
  Repair TARGET using the endemic genetic method

Available options:
  --log-loc                Add location to log messages
  --no-log-timestamp       Remove timestamps from log messages
  --log-level LOGLEVEL     The logging level to use (default: WARN)
  --log-file FILE          Append logs to FILE
  --seed INT               The random seed to use. Generated at runtime if not
                           provided.
  --config CONFIG          The configuration to use. CONF can either be a path
                           to a JSON file, or the JSON can be specified directly
  --override CONFIG        Override the configuration with the given CONFIG.
                           CONF can either be a path to a JSON file, or the JSON
                           can be specified directly
  -h,--help                Show this help text
```

The <LogLevel> parameter can be set to the following levels:

+ `TRACE` for absolutely everything (A LOT OF OUTPUT)
+ `DEBUG` to see most of what is logged (a bit less than TRACE),
+ `AUDIT` to see a time summaries for various locations
+ `VERBOSE` To see a bit more than info, but still not too much
+ `INFO` to see only informative messages and above
+ `WARN` for warnings or more
+ `ERROR` for errors only, and
+ `FATAL` for fatal errors (an irrecoverable crash)


