module Synth.Util where

import System.IO
import Control.Monad (when)
import System.Environment ( getArgs )
import Data.Char (isSpace)
import Data.Bits

import SrcLoc

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

prop_dropsPrefix :: String -> String -> Bool
prop_dropsPrefix st rest = dropPrefix st (st++rest) == rest

-- Checks if a string starts with a given prefix
startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p:ps) (s:ss) | p == s = startsWith ps ss
startsWith _ _ = False

prop_startsWith :: String -> String -> Bool
prop_startsWith st rest = startsWith st (st ++ rest) == True

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

-- Turns a list of booleans into an int
boolsToBit :: [Bool] -> Int
boolsToBit bs | length bs > size =
    error $ "Only works for lists of length <= " ++ show size
  where size = finiteBitSize (0 :: Int)
boolsToBit bs =
      (foldl (.|.) zeroBits  .
      map (bit . fst) .
      filter (\(_,x) -> x) .
      zip [0..]) bs

-- Turns an int into a list of booleans
bitToBools :: Int -> [Bool]
bitToBools b =  map (testBit b) [0.. (finiteBitSize (0 :: Int)) -1]

-- We want to be able to make SrcSpans into the ones made by `justParseExpr`,
-- which means we replace the actual filenames with "<interactive>""
mkInteractive :: SrcSpan -> SrcSpan
mkInteractive (RealSrcSpan rs) = RealSrcSpan $ mkRealSrcSpan ns ne
  where UnhelpfulSpan ic = interactiveSrcSpan
        rss = realSrcSpanStart rs
        rse = realSrcSpanEnd rs
        ns = mkRealSrcLoc ic (srcLocLine rss) (srcLocCol rss)
        ne = mkRealSrcLoc ic (srcLocLine rse) (srcLocCol rse)
mkInteractive (UnhelpfulSpan _) = interactiveSrcSpan