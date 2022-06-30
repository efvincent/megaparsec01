module RPS where

data Weapon = Rock | Paper | Scissors
  deriving (Eq, Bounded, Enum, Show)

data Winner = First | Second | Draw
  deriving (Eq, Show, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper   , Rock    ) = First
winner (Scissors, Paper   ) = First
winner (Rock    , Scissors) = First
winner (w1, w2)
  | w1 == w2  = Draw
  | otherwise = Second
