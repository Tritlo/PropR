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


More interesting for 2 2:

```
(2,2,["zero = 0 :: Int","one = 1 :: Int"],"[Int] -> Int",["prop_IsSymmetric f xs = f xs == f (reverse xs)","prop_Bin f = f [] == 0 || f [] == 1","prop_not_const f = not ((f []) == f [1,2,3])"]) fits done!
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

real	0m42.684s
user	0m32.147s
sys	0m10.634s
```
