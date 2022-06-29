{-# LANGUAGE RankNTypes #-}
module Dice2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Dice
import System.Random

-- | ∀a. Builds a state function where state is StdGen and
-- the return  from the state is type `a`
type RDieStateTo a = State StdGen a

rollDieEx :: RDieStateTo Die
rollDieEx = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie :: RDieStateTo Die
rollDie = intToDie <$> state (randomR (1, 6))

roll3' :: RDieStateTo (Die, Die, Die)
roll3' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> RDieStateTo [Die]
nDie n = replicateM n rollDie


-- Calculator State
data Calculator = Calculator
  { _amt  :: Int
  , _tape :: [String]
  }
  deriving Show

-- | my own state that tracks a log
type CalcState a = State Calculator a

initCalcState :: Calculator
initCalcState = Calculator { _amt = 0, _tape = ["Start"] }

cAdd :: Int -> CalcState ()
cAdd n = do
  calc <- get
  let m     = _amt calc
  let tape' = ("add " ++ show n) : _tape calc
  let sum   = m + n
  put $ calc { _amt = sum, _tape = tape' }

cSubtract :: Int -> CalcState ()
cSubtract n = do
  calc <- get
  put $ calc
    { _amt  = _amt calc - n
    , _tape = ("subtract " ++ show n) : _tape calc
    }

calcToString :: CalcState String
calcToString = do
  calc <- get
  let tape' = map (++ "\n") . reverse . _tape $ calc
  let amt'  = show $ _amt calc
  pure $ concat tape' ++ amt'

work :: CalcState Int
work = do
  cAdd 10
  cAdd 12
  cSubtract 18
  cAdd 3
  cSubtract 4
  cAdd 42
  cSubtract 11
  _amt <$> get

works :: CalcState String
works = do
  work
  calcToString
