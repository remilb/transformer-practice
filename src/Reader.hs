module Reader where

import           Identity
import           Trans
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

type Reader r = ReaderT r Identity

newtype ReaderT r m a = ReaderT {runReaderT :: r -> (m a)}

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT $ (\r -> return (f r))

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f rma = ReaderT $ (\r -> f $ runReaderT rma r)

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f rma = ReaderT $ runReaderT rma . f

instance Functor m => Functor (ReaderT r m) where
    fmap f ra = ReaderT (\r -> fmap f $ runReaderT ra r)

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT (\r -> pure a)
    rf <*> ra = ReaderT $ (\r -> (runReaderT rf r) <*> (runReaderT ra r))

instance Monad m => Monad (ReaderT r m) where
    return = pure
    ra >>= f = ReaderT $ \r -> do
        a <- runReaderT ra r
        runReaderT (f a) r

instance Trans (ReaderT r) where
    lift ma = ReaderT $ const ma

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

-- * Reader Ops
ask :: Monad m => ReaderT r m r
ask = ReaderT (\r -> return r)

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f (ReaderT rf) = ReaderT $ rf . f

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = fmap f ask
