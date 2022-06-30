module RLE where

{-
  Run Length Encoding Exercise solution

  "encode" sequence of two or more identical characters by
  writing the number of characters as a digit, and then the
  character being repeated.

  Assumptions:
  * only alpha characters, no numeric characters

-}

data Run
  = Single Char
  | Multi (Int, Char)
  deriving Show

rle :: String -> [Run]
rle [] = []
rle s  = go s Nothing []
 where
  go :: String -> Maybe (Char, Int) -> [Run] -> [Run]
  go [] Nothing enc = enc
  go [] (Just (cur, n)) enc
    | n > 1     = Multi (n, cur) : enc
    | otherwise = Single cur : enc
  go (c : cs) Nothing enc = go cs (Just (c, 1)) enc
  go (c : cs) (Just (cur, n)) enc
    | c == cur  = go cs (Just (cur, n + 1)) enc
    | n == 1    = go cs (Just (c, 1)) (Single cur : enc)
    | otherwise = go cs (Just (c, 1)) (Multi (n, cur) : enc)
