{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Funt.Monad where

import Language.Funt.Environment (Env)
import Control.Monad.Reader
  ( MonadIO
  , ReaderT
  , MonadReader
  , ReaderT (ReaderT))
import Control.Monad.Catch
  ( MonadCatch
  , MonadMask
  , MonadThrow)

-- | evaluation monad wrapping lexical scope based on the environment
-- of type @Env@ with IO access
newtype EvalMonad a = EvalMonad {runEvalMonad :: ReaderT Env IO a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow 
    , MonadCatch
    , MonadMask)