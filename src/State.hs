module State where

import           Identity
import           Trans
import           LiftSigs
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

type State s = StateT s Identity

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ (\s -> return $ f s)

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT sf) initState = fmap fst $ sf initState

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT (StateT sf) initState = fmap snd $ sf initState

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f (StateT sf) = StateT $ f . sf

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f (StateT sf) = StateT $ sf . f

instance Functor m => Functor (StateT s m) where
    fmap f sa = StateT
        (\s' -> let mb = runStateT sa s' in fmap (\(a, s) -> (f a, s)) mb)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT (\s -> pure (a, s))
    sf <*> sa = StateT $ \s1 -> do
        (f, s2) <- runStateT sf s1
        (a, s3) <- runStateT sa s2
        return (f a, s3)

instance Monad m => Monad (StateT s m) where
    return = pure
    sa >>= f = StateT $ \s -> do
        (a, s') <- runStateT sa s
        runStateT (f a) s'

instance Trans (StateT s) where
    lift m = StateT (\s -> fmap (\a -> (a, s)) m)

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

-- * State Ops
get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put newState = StateT $ \s -> return ((), newState)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return ((), f s)

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = StateT $ \s -> return ((), f $! s)

gets :: Monad m => (s -> a) -> StateT s m a
gets f = StateT $ \s -> return (f s, s)


-- * Special Lifts
liftCatch :: Catch e m (a, s) -> Catch e (StateT s m) a
liftCatch catch m handler =
    StateT $ \s -> catch (runStateT m s) (\e -> runStateT (handler e) s)
