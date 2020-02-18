{-# LANGUAGE TypeFamilies #-}
module State.Class where

import qualified State
import qualified Except
import qualified Reader
import qualified Writer
import           Trans

class (Monad m) => MonadState m where
    type S m
    get :: m (S m)
    put :: (S m) -> m ()

state :: MonadState m => (S m -> (a, S m)) -> m a
state f = do
    s <- get
    let (a, s') = f s
    put s'
    return a  

modify :: MonadState m => (S m -> S m) -> m ()
modify f = fmap f get >>= put

modify' :: MonadState m => (S m -> S m) -> m ()
modify' f = fmap (f $!) get >>= put

gets :: MonadState m => (S m -> a) -> m a
gets f = fmap f get

-- * Instances
instance (Monad m) => MonadState (State.StateT s m) where
    type S (State.StateT s m) = s
    get = State.get
    put = State.put

instance (MonadState m) => MonadState (Reader.ReaderT r m) where
    type S (Reader.ReaderT r m) = S m
    get = lift get
    put = lift . put

instance (Monoid w, MonadState m) => MonadState (Writer.WriterT w m) where
    type S (Writer.WriterT w m) = S m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (Except.ExceptT e m) where
    type S (Except.ExceptT e m) = S m
    get = lift get
    put = lift . put