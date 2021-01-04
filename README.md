Synthesis using GHC
===================

Works, but is pretty slow at the moment, even when doing the synthesis and
testing in parallel, but it works! Requires QuickCheck to be installed globally,
otherwise the internal synthesizer cannot run the tests (we should fix this
by some NIX magic).

The synthesis part itself is quite fast... it's the filtering of the search
space that's slow, wince we are essentially compiling every expression and
booting the whole GHC each time.

When synthesizing for `(2,3,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",["propIsSymmetric f xs = f xs == f (reverse xs)"])` the runtime was

```
real	61m5.095s
user	44m22.003s
sys	16m28.323s
```

Finding `5844` matches, including `(foldl (+) zero)` and
weirder ones (which are indeed symmetric, the only property we asked for) like
`(foldr (curry product) (id zero))`


Current output for 2 2:
```
[nix-shell:~/ghc-synth]$ time cabal run +RTS -N4
Up to date
TARGET TYPE:
  [Int] -> Int
MUST SATISFY:
  propIsSymmetric f xs = f xs == f (reverse xs)
IN CONTEXT:
  zero = 0 :: Int
  one = 1 :: Int
SYNTHESIZING...
Synthesizing (2,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",["propIsSymmetric f xs = f xs == f (reverse xs)"])
Synthesizing (0,["zero = 0 :: Int","one = 1 :: Int"],"Int -> Int -> Int",[])
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int -> Int -> Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int -> Int -> Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int -> Int -> Int",[])!
Synthesizing (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])
Synthesizing (0,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",[])
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",[])!
Synthesizing (0,["zero = 0 :: Int","one = 1 :: Int"],"[[Int] -> Int]",[])
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[[Int] -> Int]",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[[Int] -> Int]",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])!
Found (0,["zero = 0 :: Int","one = 1 :: Int"],"Int",[])!
FOUND MATCHES:
length
product
sum
foldl (-) zero
foldl (-) one
foldl (-) maxBound
foldl (-) minBound
foldl asTypeOf zero
foldl asTypeOf one
foldl asTypeOf maxBound
foldl asTypeOf minBound
foldl const zero
foldl const one
foldl const maxBound
foldl const minBound
foldl gcd zero
foldl gcd one
foldl gcd maxBound
foldl gcd minBound
foldl lcm zero
foldl (*) zero
foldl (*) one
foldl (*) maxBound
foldl (*) minBound
foldl (+) zero
foldl (+) one
foldl (+) maxBound
foldl (+) minBound
foldl max zero
foldl max one
foldl max maxBound
foldl max minBound
foldl min zero
foldl min one
foldl min maxBound
foldl min minBound
foldl return zero
foldl return one
foldl return maxBound
foldl return minBound
foldl pure zero
foldl pure one
foldl pure maxBound
foldl pure minBound
foldr subtract zero
foldr subtract one
foldr subtract maxBound
foldr subtract minBound
foldr gcd zero
foldr gcd one
foldr gcd maxBound
foldr gcd minBound
foldr lcm zero
foldr lcm minBound
foldr (*) zero
foldr (*) one
foldr (*) maxBound
foldr (*) minBound
foldr (+) zero
foldr (+) one
foldr (+) maxBound
foldr (+) minBound
foldr max zero
foldr max one
foldr max maxBound
foldr max minBound
foldr min zero
foldr min one
foldr min maxBound
foldr min minBound
foldr seq zero
foldr seq one
foldr seq maxBound
foldr seq minBound
const zero
const one
const maxBound
const minBound
($) length
($) product
($) sum
return zero
return one
return maxBound
return minBound
pure zero
pure one
pure maxBound
pure minBound
($!) length
($!) product
($!) sum
id length
id product
id sum
asTypeOf length head
asTypeOf length last
asTypeOf length length
asTypeOf length maximum
asTypeOf length minimum
asTypeOf length product
asTypeOf length sum
asTypeOf product head
asTypeOf product last
asTypeOf product length
asTypeOf product maximum
asTypeOf product minimum
asTypeOf product product
asTypeOf product sum
asTypeOf sum head
asTypeOf sum last
asTypeOf sum length
asTypeOf sum maximum
asTypeOf sum minimum
asTypeOf sum product
asTypeOf sum sum

real	0m38.105s
user	1m7.705s
sys 	0m19.140s
```

