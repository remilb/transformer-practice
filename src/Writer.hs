module Writer where

import           Identity
import           Trans
import           Data.Functor
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

type Writer w = WriterT w Identity

newtype WriterT w m a = WriterT {runWriterT :: m (w, a)}

writer :: Monad m => (w, a) -> WriterT w m a
writer wa = WriterT $ return wa

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT w = fmap fst $ runWriterT w

mapWriterT :: (m (w, a) -> n (w', b)) -> WriterT w m a -> WriterT w' n b
mapWriterT f = WriterT . f . runWriterT

instance Functor m => Functor (WriterT w m) where
    fmap f wa = WriterT $ fmap (fmap f) $ runWriterT wa

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a = WriterT $ pure (mempty, a)
    wf <*> wa = WriterT $ liftA2 (\(w1, f) (w2, a) -> (w1 <> w2, f a))
                                 (runWriterT wf)
                                 (runWriterT wa)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return = pure
    wa >>= f = WriterT $ do
        (w1, a) <- runWriterT wa
        (w2, b) <- runWriterT (f a)
        return (w1 <> w2, b)

instance Monoid w => Trans (WriterT w) where
    lift ma = WriterT $ fmap (\a -> (mempty, a)) ma

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO

-- * Writer Ops
tell :: Monad m => w -> WriterT w m ()
tell msg = WriterT $ return (msg, ())

listen :: Monad m => WriterT w m a -> WriterT w m (w, a)
listen wa = WriterT $ fmap (\(w, a) -> (w, (w, a))) (runWriterT wa)

listens :: Monad m => (w -> b) -> WriterT w m a -> WriterT w m (b, a)
listens f wa = WriterT $ fmap (\(w, a) -> (w, (f w, a))) (runWriterT wa)

pass :: Monad m => WriterT w m (w -> w, a) -> WriterT w m a
pass wa = WriterT $ do
    (w, (fw, a)) <- runWriterT wa
    return (fw w, a)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f wa = WriterT $ do
    (w, a) <- runWriterT wa
    return (f w, a)
