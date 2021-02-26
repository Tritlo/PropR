module Synth.Util where

import System.IO
import Control.Monad (when)
import System.Environment ( getArgs )
import Data.Char (isSpace)

-- Removes whitespace before and after a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Checks if the debug flag is set
hasDebug :: IO Bool
hasDebug = ("-fdebug" `elem`) <$> getArgs

-- Prints only when debug is enabled
pr_debug :: String -> IO ()
pr_debug str = do dbg <- hasDebug
                  when dbg $ putStrLn str

-- Prints a string, and then flushes, so that intermediate strings show up
putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

-- Drop a prefix of a string, i.e. dropPrefix "123" "123abc" == "abc"
dropPrefix :: String -> String -> String
dropPrefix (p:ps) (s:ss) | p == s = dropPrefix ps ss
dropPrefix _ s = s

-- Checks if a string starts with a given prefix
startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p:ps) (s:ss) | p == s = startsWith ps ss
startsWith _ _ = False


contextLet :: [String] -> String -> String
contextLet context l =
    unlines ["let"
            , unlines $ map ("    " ++)  context
            , "in " ++ l]

mapFirst :: (a -> Maybe a) -> [a] -> Maybe [a]
mapFirst = mapFirst' []
  where mapFirst' :: [a] -> (a -> Maybe a) -> [a] -> Maybe [a]
        mapFirst' sf _ [] = Nothing
        mapFirst' sf f (a:as) = case f a of
                                 Just a' -> Just $ (reverse (a':sf)) ++ as
                                 _ -> mapFirst' (a:sf) f as