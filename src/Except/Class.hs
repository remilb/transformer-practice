{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Except.Class where

import qualified Except
import qualified Reader
import qualified State
import qualified Writer
import           Trans

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

-- * Instances
instance Monad m => MonadError e (Except.ExceptT e m) where
    throwError = Except.throwError
    catchError = Except.catchError

instance (Monad m, MonadError e m) => MonadError e (Reader.ReaderT r m) where
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (Monad m, MonadError e m) => MonadError e (State.StateT s m) where
    throwError = lift . throwError
    catchError = State.liftCatch catchError

instance (Monoid w, Monad m, MonadError e m) => MonadError e (Writer.WriterT w m) where
    throwError = lift . throwError
    catchError = Writer.liftCatch catchError
