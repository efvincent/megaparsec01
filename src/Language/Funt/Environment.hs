{-# LANGUAGE FlexibleInstances #-}
module Language.Funt.Environment where

import Data.List (elemIndex)
import qualified Data.Text as T
import Language.Funt.Syntax (Term)

-- | The environment in which an expression is evaluated.
-- contains a list of local and global bindings
data Env = Env
  { _locals  :: [LocalBind]
  , _globals :: [GlobalBind]
  }
  deriving (Eq, Show)

data Binding = NameBinding
  deriving (Eq, Show)

type LocalBind = (T.Text, Binding)

type GlobalBind = (T.Text, Binding, Term)

-- | The empty environment
emptyEnv :: Env
emptyEnv = Env [] []

-- | The typeclass @HasLocals@ represents data structures w/ local bindings
class HasLocals a where
  -- | Given a variable and an environment, insert the variable into 
  -- the local environment
  pushLocal :: T.Text -> a -> a

  -- | Given a variable name and environment, return the DeBruijn index
  -- of the local variable if found
  getLocalIdx :: T.Text -> a -> Maybe Int

  -- | Given a DeBruijn index and environment, return the variable name
  getLocalName :: Int -> a -> Maybe T.Text


-- | Instance of @HasLocals@ for a list of local bindings
instance HasLocals [LocalBind] where
  pushLocal var list = (var, NameBinding) : list
  getLocalIdx var list = elemIndex var (fst <$> list)
  getLocalName idx list = (fst <$> list) `getAt` idx
   where
    getAt :: [a] -> Int -> Maybe a
    getAt [] _ = Nothing
    getAt (x : xs) n
      | n < 0     = Nothing
      | n == 0    = Just x
      | otherwise = getAt xs n

-- | The typeclass @HasGlobals@ represents data structures having
-- global bindings
class HasGlobals a where
  pushGlobal :: (T.Text, Term) -> a -> a
  getGlobalTerm :: T.Text -> a -> Maybe Term

-- | an instance of @HasGlobals@ for a list of global bindings
instance HasGlobals [GlobalBind] where
  pushGlobal (var, expr) list = (var, NameBinding, expr) : list
  getGlobalTerm var = go
   where
    go :: [GlobalBind] -> Maybe Term
    go [] = Nothing
    go ((v, _, t) : xs)
      | v == var  = Just t
      | otherwise = go xs

-- | an instance of @HasGlobal@ for the environment
instance HasGlobals Env where
  pushGlobal pair env = env { _globals = pushGlobal pair $ _globals env }
  getGlobalTerm var env = getGlobalTerm var $ _globals env
