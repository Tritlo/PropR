module Data where

data Expr = Add Int Int | Mul Int Int

evalExpr :: Expr -> Int
evalExpr (Add a b) = a + b
evalExpr (Mul a b) = a * b

plus :: Int -> Int -> Expr
plus = Mul

prop_isPlus :: Bool
prop_isPlus = evalExpr (plus 2 6) == 8

---- EXPECTED ----
-- diff --git a/tests/cases/Data.hs b/tests/cases/Data.hs
-- --- a/tests/cases/Data.hs
-- +++ b/tests/cases/Data.hs
-- @@ -6,2 +6,2 @@ evalExpr (Add a b) = a + b
--  evalExpr (Add a b) = a + b
-- -evalExpr (Mul a b) = a * b
-- +evalExpr (Mul a b) = 8
---- END EXPECTED ----