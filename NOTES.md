Game
====
State:

current type
fits for the type
candidates
refinement candidates

Actions:

pick a complete candidate to test, return it if it works
pick a refinement candidate to start expanding
go back up a level

Goal:

find function that satisfies all properties of the initial type
reward: fewest steps until match found.

```
                   length
                  /
                  |
_ :: [Int] -> Int - sum
                  |
                  |\                                  (foldl1 (+))
                  | product                          /
                  |\                                 |
                  | (foldl1 (_ :: Int -> Int -> Int) - (foldl1 (*))
                  |                                  |
                  |                                  |\
                  |                                  | (foldl1 (const _))
                  |                                  \ ...
                  |\
                  | (foldl (_ :: a -> Int -> Int) (_ :: a))
                  |
                  \ ...

```
