module Commutes where

import           State
import           Writer
import           Except
import           Reader
import           Trans

-- * Using type signatures to determine which monad transformers commute.
-- * Written as runOuterInner, where outer is the outermost monad.

-- ** StateT and WriterT -- NOT COMMUTATIVE (Or is it up to isomorphism?)
runStateWriter :: Monad m => s -> StateT s (WriterT w m) a -> m (w, (a, s))
runStateWriter s = runWriterT . ($ s) . runStateT

runWriterState :: Monad m => s -> WriterT w (StateT s m) a -> m ((w, a), s)
runWriterState s = ($ s) . runStateT . runWriterT

-- ** StateT and ExceptT -- NOT COMMUTATIVE
runStateExcept
    :: Monad m => s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runStateExcept s = runExceptT . ($ s) . runStateT

runExceptState :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runExceptState s = ($ s) . runStateT . runExceptT

-- ** StateT and ReaderT -- COMMUTATIVE!
runStateReader :: Monad m => s -> r -> StateT s (ReaderT r m) a -> m (a, s)
runStateReader s r = ($ r) . runReaderT . ($ s) . runStateT

runReaderState :: Monad m => s -> r -> ReaderT r (StateT s m) a -> m (a, s)
runReaderState s r = ($ s) . runStateT . ($ r) . runReaderT

-- ** WriterT and ExceptT -- NOT COMMUTATIVE
runWriterExcept :: Monad m => WriterT w (ExceptT e m) a -> m (Either e (w, a))
runWriterExcept = runExceptT . runWriterT

runExceptWriter :: Monad m => ExceptT e (WriterT w m) a -> m (w, Either e a)
runExceptWriter = runWriterT . runExceptT

-- ** WriterT and ReaderT -- COMMUTATIVE!
runWriterReader :: Monad m => r -> WriterT w (ReaderT r m) a -> m (w, a)
runWriterReader r = ($ r) . runReaderT . runWriterT

runReaderWriter :: Monad m => r -> ReaderT r (WriterT w m) a -> m (w, a)
runReaderWriter r = runWriterT . ($ r) . runReaderT

-- ** ExceptT and ReaderT -- COMMUTATIVE!
runExceptReader :: Monad m => r -> ExceptT e (ReaderT r m) a -> m (Either e a)
runExceptReader r = ($ r) . runReaderT . runExceptT

runReaderExcept :: Monad m => r -> ReaderT r (ExceptT e m) a -> m (Either e a)
runReaderExcept r = runExceptT . ($ r) . runReaderT
