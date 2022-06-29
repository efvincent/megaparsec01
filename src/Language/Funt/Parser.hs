{-# LANGUAGE OverloadedStrings #-}

module Language.Funt.Parser where

import Control.Applicative ((<|>), Alternative(empty, many))
import Control.Monad ()
import Control.Monad.State (modify)
import Control.Monad.State.Strict
  (MonadState(get, put), State, StateT(runStateT), runState)
import Data.Functor.Identity (Identity)
import Data.List (foldl1')
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.TypeLits (ErrorMessage(Text))
import Language.Funt.Environment
import Language.Funt.Monad (EvalMonad(..))
import Language.Funt.Syntax (Literal(..), Term(..))
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
  , some
  )
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Byte as M
import Text.Megaparsec.Char (alphaNumChar, asciiChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (incorrectIndent)
import Text.Megaparsec.Debug (dbg)

type Parser = ParsecT Void Text EvalMonad

data Span = Span SourcePos SourcePos
data Spanned a = Spanned Span a

-- | augment a parser with a source span
spanned :: Parser (a, SourcePos) -> Parser (Spanned a)
spanned parser = do
  start    <- getSourcePos
  (x, end) <- parser
  pure (Spanned (Span start end) x)

-- | consume whitespace following a lexeme, but record its endpoint
-- as being before the whitespace
lexemeP :: Parser a -> Parser (a, SourcePos)
lexemeP parser = (,) <$> parser <*> (getSourcePos <* ws)

-- | Whitespace - parse and skip
ws :: Parser ()
ws =
  L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "/*" "*/")

-- | creates a parser for the string that also consumes trailing whitespace
symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

-- | parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | wraps a parser enhancing it to consume trailing whitespace  
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- | parse a natural number
pNat :: Parser Integer
pNat = lexeme L.decimal

-- | parse a natural number returning the position in source
pNatP :: Parser (Integer, SourcePos)
pNatP = lexemeP L.decimal

-- | parse an integer
pInt :: Parser Integer
pInt = L.signed ws pNat

-- | unsigned floating point
pUFloat :: Parser Double
pUFloat = lexeme L.float

-- | signed float
pFloat :: Parser Double
pFloat = L.signed ws pUFloat

-- | parse an identifier
pIdent :: Parser T.Text
pIdent = lexeme $ do
  ident <- some letterChar
  pure $ T.pack ident

-- | parse a literal value
pLiteral :: Parser Term
pLiteral = choice
  [ parens pTerm
  , Value (LitBool True) <$ symbol "True"
  , Value (LitBool False) <$ symbol "False"
  , Value (LitNat 0) <$ symbol "0"
  ]

pApplication :: Parser Term
pApplication = do
  parameters <- some pLiteral
  pure $ foldl1' App parameters

pExpr :: Parser Term
pExpr = choice
  [ parens pTerm
  , Succ <$ symbol "succ" <*> label err pApplication
  , Pred <$ symbol "pred" <*> label err pApplication
  , IsZero <$ symbol "iszero" <*> label err pApplication
  , pIf
  , pAbs
  , pLiteral
  ]
 where
  err :: String
  err = "function application, literal, or term in parens"

-- | Parse a term
pTerm :: Parser Term
pTerm = do
  apps <- some pExpr
  pure $ foldl1' App apps

-- | Parse an abstraction
pAbs :: Parser Term
pAbs = do
  _     <- symbol "\\"
  ident <- pIdent
  _     <- symbol "."
  Abs ident <$> pTerm

-- | parse an if then else statement
pIf :: Parser Term
pIf = do
  _     <- symbol "if"
  cond  <- pExpr
  _     <- symbol "then"
  tExpr <- pExpr
  _     <- symbol "else"
  IfThenElse cond tExpr <$> pExpr

-------------- State Example -------------

-- | The type of parsers that evalute text and carry 
-- a string as state
type ParserS = ParsecT Void Text (State String)

-- | A parser that parses input as a string, and also
-- updates state with an indication of which branch it
-- chose during the parse, either branc A or B.
-- If the input is "foo" then it will put "branch A" in
-- state. If not, then it will replace state with "branch B" (?)
parser0 :: ParserS String
parser0 = a <|> b
 where
  a = "foo" <$ put "branch A"
  b = get <* put "branch B"

-- | A parser that parses input as a string, and also
-- update state with an indication of which branch it
-- chose during the parse, either A or B
parser1 :: ParserS String
parser1 = a <|> b
 where
  a = "foo" <$ put "branch A" <* empty
  b = get <* put "branch B"

mainS :: IO ()
mainS = do
  let
    -- @runState@ runs a state monad replacing the state
    -- with the constant value "initial"
    run p = runState (runParserT p "" "") "initial"
    (Right a0, s0) = run parser0
    (Right a1, s1) = run parser1

  putStrLn "Parser 0"
  putStrLn ("Result:      " ++ show a0)
  putStrLn ("Final state: " ++ show s0)

  putStrLn "\nParser 1"
  putStrLn ("Result:      " ++ show a1)
  putStrLn ("Final state: " ++ show s1)

-------------- Parser in State Example -------------

type ParserS2 = StateT String (ParsecT Void Text Identity)

parser2 :: ParserS2 String
parser2 = a <|> b
 where
  a = "foo" <$ put "branch A" <* empty
  b = get <* put "branch B"

mainS2 :: IO ()
mainS2 = do
  let
    p            = runStateT parser2 "initial"
    Right (a, s) = runParser p "" ""
  putStrLn ("Result:      " ++ show a)
  putStrLn ("Final State: " ++ show s)


