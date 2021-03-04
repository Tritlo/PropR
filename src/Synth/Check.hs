module Synth.Check where

import Synth.Util
import Synth.Types

import Data.List (intercalate)

qcArgs = "stdArgs { chatty = False, maxShrinks = 0}"
qcTime = 1000000
checkImports = [ "import Test.QuickCheck" ]

buildCheckExprAtTy :: [RProp] -> RContext -> RType -> RExpr -> RExpr
buildCheckExprAtTy props context ty expr =
     "let {" ++
       (intercalate "; " . concatMap lines $
         ("qc__ = " ++ qcArgs):context
         ++ props
         ++ [ "expr__ :: " ++ ty
            , "expr__ = "++  expr
            , "propsToCheck__ = [ " ++
                 (intercalate ", " $ map propCheckExpr propNames) ++ "]" ])
     ++ "} in ((sequence propsToCheck__) :: IO [Bool])"
   where propNames = map (head . words) props
         -- We can't consolidate this into check__, since the type
         -- will be different!
         propCheckExpr pname = "isSuccess <$> " ++ propCheck pname

-- Builds the actual check.
propCheck :: String -> String
propCheck pname = "quickCheckWithResult qc__ (within "
                ++ show qcTime ++ " (" ++ pname ++ " expr__ ))"

-- The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleExpr :: [RProp] -> RContext -> RType -> RExpr -> RExpr
buildCounterExampleExpr [prop] context ty expr =
     "let {" ++
       (intercalate "; " . concatMap lines $
           ("qc__ = "  ++ qcArgs):context
           ++ [ addWithin prop
              , "expr__ :: " ++ ty
              , "expr__ = "++  expr
              , "failureToMaybe :: Result -> Maybe [String]"
              , "failureToMaybe (Failure {failingTestCase = s}) = Just s"
              , "failureToMaybe _ = Nothing"
              , "propToCheck__ = failureToMaybe <$> " ++ propCheck propName])
      ++ "} in (propToCheck__) :: IO (Maybe [String])"
   where propName = head $ words prop
         -- We can't consolidate this into check__, since the type
         -- will be different!
         qcArgs = "stdArgs { chatty = False }"
         -- We have to have the within within the prop, otherwise we
         -- don't get the arguments used.
         addWithin prop = s ++ "= within " ++ (show qcTime) ++"(" ++ e ++ ")"
           where (s,('=':e)) = break ((==) '=') prop

buildCounterExampleExpr _ _ _ _ = error "bCEE only works for one prop at a time!"
