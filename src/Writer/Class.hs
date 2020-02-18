{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Writer.Class where

import qualified Writer
import qualified State
import qualified Except
import qualified Reader
import           Trans

class (Monoid (W m), Monad m) => MonadWriter m where
    type W m
    tell :: W m -> m ()
    listen :: m a -> m (W m, a)
    pass :: m (W m -> W m, a) -> m a

listens :: MonadWriter m => (W m -> b) -> m a -> m (b, a)
listens f m = do
    (w, a) <- listen m
    return (f w, a)

censor :: MonadWriter m => (W m -> W m) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (f, a)

-- * Instances
instance (Monad m, Monoid w) => MonadWriter (Writer.WriterT w m) where
    type W (Writer.WriterT w m) = w
    tell   = Writer.tell
    listen = Writer.listen
    pass   = Writer.pass

instance (MonadWriter m) => MonadWriter (Reader.ReaderT r m) where
    type W (Reader.ReaderT r m) = W m
    tell   = lift . tell
    listen = Reader.mapReaderT listen
    pass   = Reader.mapReaderT pass

instance (MonadWriter m) => MonadWriter (State.StateT s m) where
    type W (State.StateT s m) = W m
    tell   = lift . tell
    listen = State.liftListen listen
    pass   = State.liftPass pass

instance (MonadWriter m) => MonadWriter (Except.ExceptT e m) where
    type W (Except.ExceptT e m) = W m
    tell   = lift . tell
    listen = Except.liftListen listen
    pass   = Except.liftPass pass
