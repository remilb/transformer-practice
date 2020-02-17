{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Reader.Class where

import qualified Reader                        as Reader
import           State
import           Except
import           Writer
import           Trans

class Monad m => MonadReader r m | m -> r where
    reader :: (r -> a) -> m a
    ask :: m r
    local :: (r -> r) -> m a -> m a

asks :: (MonadReader r m) => (r -> a) -> m a
asks = reader

-- * Instances
instance Monad m => MonadReader r (Reader.ReaderT r m) where
    reader = Reader.reader
    ask    = Reader.ask
    local  = Reader.local

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    reader :: (r -> a) -> (WriterT w m) a
    reader = lift . reader
    ask :: (WriterT w m) r
    ask = lift ask
    local :: (r -> r) -> (WriterT w m) a -> (WriterT w m) a
    local = mapWriterT . local

instance (MonadReader r m) => MonadReader r (StateT s m) where
    reader = lift . reader
    ask    = lift ask
    local  = mapStateT . local

instance (MonadReader r m) => MonadReader r (ExceptT e m) where
    reader = lift . reader
    ask    = lift ask
    local  = mapExceptT . local
