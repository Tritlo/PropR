[![Build, test, and deploy](https://github.com/Tritlo/PropR/actions/workflows/build-test-deploy.yml/badge.svg)](https://github.com/Tritlo/PropR/actions/workflows/build-test-deploy.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-hotpink.svg)](https://github.com/Tritlo/PropR/blob/master/LICENSE)
[![DOI](https://zenodo.org/badge/315950874.svg)](https://zenodo.org/badge/latestdoi/315950874)

# PropR: Property-based Program repair using GHC 🐣

> Inspired by GenProg.

Welcome! This repository holds *PropR*, a program repair library in and for Haskell. 
With just a few dependencies, we can help you repair your program using GHC and genetic search! 

Key aspects:

- Module-Level Repair
- QuickCheck & Tasty-TestTrees are supported
- Genetic, Random and Exhaustive Search Algorithms
- Output of the Repair is a git-patch

We are happy if you give it a try and reach out to us - feel free to open an issue with feedback or drop us an email. 

The repair process uses arcane knowledge that yet has to be molded into a publication. 
To give a coarse-grained overview, we utilize punching typed holes in the expressions touched by failing properties, 
and ask GHC for valid hole-fits. 
The compilers suggestions are used as components for the genetic search, where we mix and match them until all properties are passed.

## Requirements


| Haskell-Way   |   Docker-Way  |   Nix-Way  | 
| ------------- | ------------- | ---------- | 
| GHC 8.10      | Docker 20+    | Nix 2.3+   |
| Cabal 1.24    | Compose 2.2   |    ...     |

These are the versions we currently use - older versions might work too, please let us know if you could use different versions!

## Usage
-----
This program requires version `3.4` or higher of `cabal`, and version `8.10`  of `ghc`.
A complete environment required to run (minus the `QuickCheck`)
is defined in `shell.nix`, and can be activated using `nix-shell`, if installed.

**MacOS** and other users whose installation does not do dynamic linking by default have to use `cabal configure --enable-optimization --enable-executable-dynamic`. 
Otherwise you might run into
```
GHCERROR: cannot find object file ‘[...]/BrokenModule.dyn_o’ while linking an interpreted expression
```

Windows users might find some help [here](https://www.linux.org/pages/download/).

To run the program, ensure that the `check-helpers` library is installed by running
`$ cabal install --lib check-helpers`. You can then run

```
$ cabal run propr -- examples/BrokenModule.hs
```
or using a config:

```
$ cabal run propr -- --config resources/test_config.json --log-level TRACE examples/FourFixes.hs
```

Optionally, you can use Docker: 

```
$ docker build -t propr .
$ docker run -it --volume $(pwd):/data --entrypoint /app/propr propr /data/examples/BrokenModule.hs
```

or docker-compose:
```
$ docker-compose up --build
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
$ cabal run propr -- examples/BrokenGCD.hs
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

## Configuration
---------

```
PropR - Genetic program repair for Haskell

Usage: propr [--log-loc] [--no-log-timestamp] [--log-level LOGLEVEL]
               [--log-file FILE] [--seed INT] [--config CONFIG]
               [--override CONFIG] TARGET
  Repair TARGET using the propr genetic method

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

You can also find a lot example configurations in [resources](./resources).

The program will tell you if you are using wrong key-words in your config, 
and with `VERBOSE` you will see a print of the configuration once the program starts.
