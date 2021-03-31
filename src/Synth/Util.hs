module Synth.Util where

import Control.Exception (assert)
import Control.Monad (when)
import Data.Bifunctor (second)
import Data.Bits
import Data.Char (isSpace, toUpper)
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import GHC
import GHC.Stack (callStack, getCallStack, withFrozenCallStack)
import qualified GHC.Stack as GHS
import GhcPlugins (HasCallStack, Outputable (ppr), fsLit, mkVarUnqual, showSDocUnsafe)
import SrcLoc
import Synth.Types
import System.CPUTime
import System.Environment (getArgs)
import System.IO
import Text.Printf (printf)

progAtTy :: EExpr -> EType -> EExpr
progAtTy e_prog e_ty =
  noLoc $ ExprWithTySig NoExtField (noLoc $ HsPar NoExtField e_prog) e_ty

undefVar :: HsExpr GhcPs
undefVar = HsVar NoExtField $ noLoc $ mkVarUnqual $ fsLit "undefined"

-- Removes whitespace before and after a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Checks if the debug flag is set
hasDebug :: IO Bool
hasDebug = ("-fdebug" `elem`) <$> getArgs

data LogLevel
  = DEBUG
  | AUDIT
  | INFO
  | WARN
  | ERROR
  | FATAL
  deriving (Eq, Ord, Read, Show)

logLevel :: IO LogLevel
logLevel = do
  args <- Map.fromList . map (second (map toUpper . drop 1) . break (== '=')) <$> getArgs
  return $ case args Map.!? "--log" of
    Just lvl -> read lvl
    _ -> ERROR

split :: Eq a => a -> [a] -> [[a]]
split a [] = []
split a as =
  t : case r of
    [] -> []
    _ : rs -> split a rs
  where
    (t, r) = break (a ==) as

logStr :: HasCallStack => LogLevel -> String -> IO ()
logStr olvl str =
  do
    lvl <- logLevel
    when (olvl >= lvl) $ do
      let (loc : _) = map snd $ getCallStack callStack
          sfile = split '/' $ GHS.srcLocFile loc
          (i, l) = assert (not (null sfile) && not (any null sfile)) (init sfile, last sfile)
          sfileRes = intercalate "/" (map (take 1) i ++ [l])
          sline = show (GHS.srcLocStartLine loc)
      showLoc <- ("--log-loc" `elem`) <$> getArgs
      let locO = if showLoc then "<" ++ sfileRes ++ ":" ++ sline ++ "> " else ""
      putStrLn $ locO ++ show olvl ++ ": " ++ str

logOut :: (HasCallStack, Outputable p) => LogLevel -> p -> IO ()
logOut olvl = withFrozenCallStack . logStr olvl . showUnsafe

showUnsafe :: Outputable p => p -> String
showUnsafe = showSDocUnsafe . ppr

-- Prints a string, and then flushes, so that intermediate strings show up
putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

-- Drop a prefix of a string, i.e. dropPrefix "123" "123abc" == "abc"
dropPrefix :: String -> String -> String
dropPrefix (p : ps) (s : ss) | p == s = dropPrefix ps ss
dropPrefix _ s = s

prop_dropsPrefix :: String -> String -> Bool
prop_dropsPrefix st rest = dropPrefix st (st ++ rest) == rest

-- Checks if a string starts with a given prefix
startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p : ps) (s : ss) | p == s = startsWith ps ss
startsWith _ _ = False

prop_startsWith :: String -> String -> Bool
prop_startsWith st rest = startsWith st (st ++ rest)

contextLet :: [String] -> String -> String
contextLet context l =
  "let {" ++ intercalate "; " (concatMap lines context) ++ "} in " ++ l

mapFirst :: (a -> Maybe (b, a)) -> [a] -> Maybe (b, [a])
mapFirst = mapFirst' []
  where
    mapFirst' :: [a] -> (a -> Maybe (b, a)) -> [a] -> Maybe (b, [a])
    mapFirst' sf _ [] = Nothing
    mapFirst' sf f (a : as) = case f a of
      Just (b, a') -> Just (b, reverse (a' : sf) ++ as)
      _ -> mapFirst' (a : sf) f as

-- Turns a list of booleans into an int
boolsToBit :: [Bool] -> Int
boolsToBit bs
  | length bs > size =
    error $ "Only works for lists of length <= " ++ show size
  where
    size = finiteBitSize (0 :: Int)
boolsToBit bs = (foldl (.|.) zeroBits . map (bit . fst) . filter snd . zip [0 ..]) bs

-- Turns an int into a list of booleans
bitToBools :: Int -> [Bool]
bitToBools b = map (testBit b) [0 .. finiteBitSize (0 :: Int) -1]

-- We want to be able to make SrcSpans into the ones made by `justParseExpr`,
-- which means we replace the actual filenames with "<interactive>""
mkInteractive :: SrcSpan -> SrcSpan
mkInteractive (RealSrcSpan rs) = RealSrcSpan $ mkRealSrcSpan ns ne
  where
    UnhelpfulSpan ic = interactiveSrcSpan
    rss = realSrcSpanStart rs
    rse = realSrcSpanEnd rs
    ns = mkRealSrcLoc ic (srcLocLine rss) (srcLocCol rss)
    ne = mkRealSrcLoc ic (srcLocLine rse) (srcLocCol rse)
mkInteractive (UnhelpfulSpan _) = interactiveSrcSpan

-- Every possible split of a list, and where we took the element from.
oneAndRest :: [a] -> [(a, Int, [a])]
oneAndRest = oneAndRest' 0
  where
    oneAndRest' _ [] = []
    oneAndRest' n (x : xs) =
      (x, n, xs) : map (\(y, n', ys) -> (y, n', x : ys)) (oneAndRest' (n + 1) xs)

prop_oneAndRest :: Ord a => [a] -> Bool
prop_oneAndRest xs = prop_hasAll && prop_restIsThere && prop_fromCorrectPos
  where
    app = oneAndRest xs
    prop_hasAll =
      length xs == length app
        && sort xs == sort (map (\(x, _, _) -> x) app)
    prop_restIsThere = all (\(_, _, xs) -> length xs == (length app - 1)) app
    prop_fromCorrectPos = all (\(x, i, _) -> (xs !! i) == x) app

-- Inserts the given element at the given index in the list, or at the end
insertAt :: Int -> a -> [a] -> [a]
insertAt _ a [] = [a]
insertAt 0 a as = a : as
insertAt n a (x : xs) = x : insertAt (n -1) a xs

-- Applies the given function to each element in the list and leaves the
-- others untouched, for each element.
applToEach :: (a -> [(l, a)]) -> [a] -> [(l, [a])]
applToEach f as = concatMap applToOne $ oneAndRest as
  where
    applToOne (g, i, r) = map (\(l', g') -> (l', insertAt i g' r)) (f g)

showTime :: Integer -> String
showTime time =
  if res > 1000
    then printf "%.2f" ((fromIntegral res * 1e-3) :: Double) ++ "s"
    else show res ++ "ms"
  where
    res :: Integer
    res = floor $ fromIntegral time * 1e-9

time :: IO a -> IO (Integer, a)
time act = do
  start <- getCPUTime
  r <- act
  done <- getCPUTime
  return (done - start, r)

repTime :: HasCallStack => IO a -> IO a
repTime act =
  do
    (t, r) <- time act
    withFrozenCallStack $ logStr WARN (showTime t)
    return r