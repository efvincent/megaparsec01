{-# LANGUAGE LambdaCase #-}
module Dice where

import System.Random

data Die
  = DOne
  | DTwo
  | DThree
  | DFour
  | DFive
  | DSix
  deriving (Eq, Show)

intToDie :: Integer -> Die
intToDie = \case
  1 -> DOne
  2 -> DTwo
  3 -> DThree
  4 -> DFour
  5 -> DFive
  6 -> DSix
  n -> error $ "intToDie got a non 1-6 integer: " ++ show n

roll3 :: (Die, Die, Die)
roll3 =
  let
    s        = mkStdGen 1337
    (d1, s1) = randomR (1, 6) s
    (d2, s2) = randomR (1, 6) s1
    (d3, _ ) = randomR (1, 6) s2
  in (intToDie d1, intToDie d2, intToDie d3)
