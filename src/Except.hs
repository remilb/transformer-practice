module Except where

import           Identity
import           Trans
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

type Except e = ExceptT e Identity

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

except :: Monad m => Either e a -> ExceptT e m a
except e = ExceptT $ return e

mapExceptT
    :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = ExceptT . fmap f' . runExceptT
  where
    f' (Left  e) = Left $ f e
    f' (Right a) = Right a

instance Functor m => Functor (ExceptT e m) where
    fmap f (ExceptT ma) = ExceptT (fmap (fmap f) ma)

instance Monad m => Applicative (ExceptT e m) where
    pure a = ExceptT (pure $ Right a)
    ExceptT mf <*> ExceptT ma = ExceptT $ do
        f <- mf
        case f of
            Left  err -> return $ Left err
            Right f'  -> do
                a <- ma
                case a of
                    Left  err -> return $ Left err
                    Right a'  -> return $ Right (f' a')


instance Monad m => Monad (ExceptT e m) where
    return = pure
    (ExceptT m) >>= f =
        ExceptT
            $   m
            >>= (\ea -> case ea of
                    Left  err -> return $ Left err
                    Right a   -> runExceptT $ f a
                )

instance Trans (ExceptT e) where
    lift m = ExceptT $ fmap Right m

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = lift . liftIO

-- * Exception ops

throwError :: Monad m => e -> ExceptT e m a
throwError = ExceptT . return . Left

catchError
    :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
catchError m handler = ExceptT $ do
    a <- runExceptT m
    case a of
        Right a' -> return $ Right a'
        Left  e  -> runExceptT $ handler e
