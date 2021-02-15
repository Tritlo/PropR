module Synth.Util where

import System.IO
import Control.Monad (when)
import System.Environment ( getArgs )
import Data.Char (isSpace)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

hasDebug :: IO Bool
hasDebug = ("-fdebug" `elem`) <$> getArgs

pr_debug :: String -> IO ()
pr_debug str = do dbg <- hasDebug
                  when dbg $ putStrLn str

putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

dropPrefix :: String -> String -> String
dropPrefix (p:ps) (s:ss) | p == s = dropPrefix ps ss
dropPrefix _ s = s

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p:ps) (s:ss) | p == s = startsWith ps ss
startsWith _ _ = False


contextLet :: [String] -> String -> String
contextLet context l =
    unlines ["let"
            , unlines $ map ("    " ++)  context
            , "in " ++ l]