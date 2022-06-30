{-# LANGUAGE OverloadedStrings #-}

module MegaState.MegaStateParser where

import Control.Monad.State ()
import Control.Monad ()
import Control.Monad.State.Strict
  (MonadState(get,put), State, StateT(runStateT), runState)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)

import Text.Megaparsec
  ( MonadParsec(label, token)
  , Parsec
  , ParsecT
  , SourcePos
  , Stream(Token)
  , between
  , choice
  , getSourcePos
  , runParser
  , runParserT
  , some)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (incorrectIndent)
import Text.Megaparsec.Debug (dbg)

type ParserS = ParsecT Void Text (State String)

efv = "eric"