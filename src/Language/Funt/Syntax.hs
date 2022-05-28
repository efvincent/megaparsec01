module Language.Funt.Syntax
  ( Literal(..)
  , Term(..)) where

import Data.Text (Text)
import GHC.IO.Device (SeekMode(AbsoluteSeek))

data Literal
  = LitBool   Bool
  | LitNat    Integer
  | LitChar   Char
  | LitString Text
  deriving (Eq, Show)

data Term
  = Value      Literal
  | IfThenElse Term Term Term
  | Succ       Term
  | Pred       Term
  | IsZero     Term
  | Abs        Text Term
  | App        Term Term
  deriving (Eq, Show)