module Synth.Check where

import Synth.Util

import Data.List (intercalate)

qcArgs = "stdArgs { chatty = False, maxShrinks = 0}"
qcImport = "import Test.QuickCheck"
buildCheckExprAtTy :: [String] -> [String] -> String -> String -> String
buildCheckExprAtTy props context ty expr =
     unlines [
         "let qc__ = "  ++ qcArgs
       , "    -- Context"
       , unlines (map ("    " ++) context)
       , "    -- Properties"
       , unlines (map ("    " ++) props)
       , "    expr__ :: " ++ ty
       , "    expr__ = "++  expr
       , "    propsToCheck__ = [ " ++
                 (intercalate
       "\n                     , " $ map propCheckExpr propNames) ++ "]"
       , "in ((sequence propsToCheck__) :: IO [Bool])"]
   where propNames = map (head . words) props
         -- We can't consolidate this into check__, since the type
         -- will be different!
         propCheckExpr pname = "isSuccess <$> quickCheckWithResult qc__ ("
                            ++ pname ++ " expr__ )"
         propToLet p = "    " ++ p