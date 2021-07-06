{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Synth.Util
-- Description : Contains general and other orphaned functions of Endemic
-- License     : MIT
-- Stability   : experimental
-- Your everyday Util file.
-- Most of the functions contained are about logging.
-- This is a pure module.
module Synth.Util where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.Bits
import Data.Char (isSpace, toUpper)
import Data.Function (on)
import Data.IORef
import Data.List (groupBy, intercalate, sort)
import qualified Data.Map as Map
import GHC
import GHC.IO.Unsafe (unsafePerformIO)
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

-- | Removes whitespace before and after a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Checks if the debug flag is set
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

-- | Splits a list by a given element.
-- The splitting element is not included in the created lists.  This could be
-- provided by libraries, but we didn't want to introduce a dependency for
-- 6 lines of code (this is not JS).
split :: Eq a => a -> [a] -> [[a]]
split a [] = []
split a as =
  t : case r of
    [] -> []
    _ : rs -> split a rs
  where
    (t, r) = break (a ==) as

logStr :: HasCallStack => LogLevel -> String -> IO ()
logStr olvl str = do
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

-- | We want to be able to make SrcSpans into the ones made by `justParseExpr`,
-- which means we replace the actual filenames with "<interactive>".
mkInteractive :: SrcSpan -> SrcSpan
-- Case 1: We have a real source Span
mkInteractive (RealSrcSpan rs) = RealSrcSpan $ mkRealSrcSpan ns ne
  where
    -- Make a lookup for the old span but use the interactive for further computing

    UnhelpfulSpan ic = interactiveSrcSpan
    rss = realSrcSpanStart rs
    rse = realSrcSpanEnd rs
    ns = mkRealSrcLoc ic (srcLocLine rss) (srcLocCol rss)
    ne = mkRealSrcLoc ic (srcLocLine rse) (srcLocCol rse)
-- Case 2: The source span was interactive or other anyway
mkInteractive (UnhelpfulSpan _) = interactiveSrcSpan

-- | Inserts the given element at the given index in the list, or at the end
insertAt ::
  -- | the index at which the element should be inserted (0 is head)
  Int ->
  -- | the element to be inserted
  a ->
  -- | the list in which to insert
  [a] ->
  -- | the list with the new element at given index,
  --   or at the end if the given index was out of list.
  [a]
insertAt _ a [] = [a]
insertAt 0 a as = a : as
insertAt n a (x : xs) = x : insertAt (n -1) a xs

-- | Transforms time given in ns (as measured by "time") into a string
showTime :: Integer -> String
showTime time =
  if res > 1000
    then printf "%.2f" ((fromIntegral res * 1e-3) :: Double) ++ "s"
    else show res ++ "ms"
  where
    res :: Integer
    res = floor $ fromIntegral time * 1e-9

-- | Stopwatch for a given function, measures the time taken by a given act.
time :: MonadIO m => m a -> m (Integer, a)
time act = do
  start <- liftIO getCPUTime
  r <- act
  done <- liftIO getCPUTime
  return (done - start, r)

statsRef :: IORef (Map.Map (String, Int) Integer)
{-# NOINLINE statsRef #-}
statsRef = unsafePerformIO $ newIORef Map.empty

collectStats :: (MonadIO m, HasCallStack) => m a -> m a
collectStats a = do
  (t, r) <- time a
  let ((_, GHS.SrcLoc {..}) : _) = getCallStack callStack
  liftIO $ modifyIORef statsRef (Map.insertWith (+) (srcLocFile, srcLocStartLine) t)
  withFrozenCallStack $ liftIO $ logStr AUDIT (showTime t)
  return r

reportStats :: MonadIO m => m ()
reportStats = liftIO $ do
  logStr AUDIT "SUMMARY"
  res <- Map.toList <$> readIORef statsRef
  let pp ((f, l), t) = "<" ++ f ++ ":" ++ show l ++ "> " ++ showTime t
  mapM_ (logStr AUDIT . pp) res

reportStats' :: MonadIO m => LogLevel -> m ()
reportStats' lvl = liftIO $ do
  logStr lvl "SUMMARY"
  res <- Map.toList <$> readIORef statsRef
  let pp ((f, l), t) = "<" ++ f ++ ":" ++ show l ++ "> " ++ showTime t
  mapM_ (logStr lvl . pp) res
