module Rdr where

import Control.Monad.Reader (MonadReader(..), Reader, asks, liftM)
import Data.Maybe (fromMaybe)
-- | The abstract syntax of a template
data Template
  = T String                  -- ^ Text
  | V Template                -- ^ Variable
  | Q Template                -- ^ Quote
  | I Template [Definition]   -- ^ Include
  | C [Template]              -- ^ Compound
  deriving (Show, Eq)

data Definition = D Template Template
  deriving (Show, Eq)

data Environment = Env
  { templates :: [(String, Template)]
  , variables :: [(String, String)]
  }
  deriving (Show, Eq)

lookupVar :: String -> Environment -> Maybe String
lookupVar name env = lookup name (variables env)

lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = lookup name (templates env)

addDefs :: [(String, String)] -> Environment -> Environment
addDefs defs env = env { variables = defs ++ variables env }

resolveDef :: Definition -> Reader Environment (String, String)
resolveDef (D t d) = do
  name  <- resolve t
  value <- resolve d
  return (name, value)

resolve :: Template -> Reader Environment String
resolve (T s) = return s
resolve (V t) = do
  varName  <- resolve t
  varValue <- asks (lookupVar varName)
  return $ fromMaybe "" varValue
resolve (Q t) = do
  tname <- resolve t
  body  <- asks (lookupTemplate tname)
  return $ maybe "" show body
resolve (I t ds) = do
  tname <- resolve t
  body  <- asks (lookupTemplate tname)
  case body of
    Just t' -> do
      defs <- mapM resolveDef ds
      local (addDefs defs) (resolve t')
    Nothing -> return ""
resolve (C ts) = fmap concat (mapM resolve ts)

