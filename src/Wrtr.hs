{-# LANGUAGE InstanceSigs #-}
module Wrtr where
import Control.Monad.Writer (MonadWriter(tell), Writer)
import Data.Char (isAlphaNum, toUpper)

inp :: String
inp = "able was I ere I saw elba practical foundations programming languages"

data Item = Item
  { idx  :: Int
  , word :: Int
  }

data LogEntry = LogEntry
  { line     :: Int
  , severity :: Severity
  , msg      :: String
  }
  deriving Eq

data Severity = Debug | Info | Warning | Error deriving (Show, Eq, Ord)
type LineNo = Int
type Result = Int

instance Show LogEntry where
  show :: LogEntry -> String
  show LogEntry { line = l, severity = s, msg = m } =
    "(" ++ show l ++ ") " ++ map toUpper (show s) ++ ": " ++ m

procWord :: String -> LineNo -> Writer [LogEntry] Result
procWord "" ln = pure 0
procWord s ln
  | length s < 4 = do
    tell [LogEntry ln Warning ("Short Word Ignored: '" ++ s ++ "'")]
    pure 0
  | length s > 10 = do
    tell [LogEntry ln Error ("Word too long: '" ++ s ++ "'")]
    pure 0
  | all isAlphaNum s = do
    tell [LogEntry ln Info ("Processed: '" ++ s ++ "'")]
    pure $ length s
  | otherwise = do
    tell [LogEntry ln Error ("Invalid Characters: '" ++ s ++ "'")]
    pure 0

procBatch :: String -> Writer [LogEntry] Result
procBatch s = do
  tell [LogEntry 0 Info "processing batch"]
  let ss' = zip [1 ..] (words s) :: [(Int, String)]
  results <- traverse (uncurry . flip $ procWord) ss'
  let ln = length results
  tell [LogEntry (ln + 1) Info "Batch Complete"]
  pure $ sum results

putLog :: (Severity -> Bool) -> [LogEntry] -> IO ()
putLog pred ls = mapM_ print (filter (pred . severity) ls)

putStrs :: [String] -> IO ()
putStrs []       = putStrLn "Batch Complete"
putStrs (s : ss) = do
  putStrLn s
  putStrs ss
