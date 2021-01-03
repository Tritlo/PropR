Synthesis using GHC
===================

Works, but is pretty slow at the moment, even when doing the synthesis and
testing in parallel, but it works!

Current output:

```
[nix-shell:~/ghc-synth]$ time cabal run
Build profile: -w ghc-8.10.2 -O1
In order, the following will be built (use -v for more details):
 - ghc-synth-0.0.1 (exe:ghc-synth) (file src/Main.hs changed)
Preprocessing executable 'ghc-synth' for ghc-synth-0.0.1..
Building executable 'ghc-synth' for ghc-synth-0.0.1..
[1 of 1] Compiling Main             ( src/Main.hs, /home/tritlo/ghc-synth/dist-newstyle/build/x86_64-linux/ghc-8.10.2/ghc-synth-0.0.1/x/ghc-synth/build/ghc-synth/ghc-synth-tmp/Main.o )
Linking /home/tritlo/ghc-synth/dist-newstyle/build/x86_64-linux/ghc-8.10.2/ghc-synth-0.0.1/x/ghc-synth/build/ghc-synth/ghc-synth ...
TARGET TYPE:
  [Int] -> Int
MUST SATISFY:
  propIsSymmetric f xs = f xs == f (reverse xs)
IN CONTEXT:
  zero = 0 :: Int
  one = 1 :: Int
SYNTHESIZING...
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

real	0m45.694s
user	1m23.086s
sys	    0m24.245s
```