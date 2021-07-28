module Data where

data Expr = Add Int Int | Mul Int Int

evalExpr :: Expr -> Int
evalExpr (Add a b) = a + b
evalExpr (Mul a b) = a * b

plus :: Int -> Int -> Expr
plus = Mul

prop_isPlus :: Bool
prop_isPlus = evalExpr (plus 2 6) == 8