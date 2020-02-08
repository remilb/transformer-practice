module Except where

import           Identity
import           Trans
import           Data.Functor
import           Control.Applicative
import           Control.Monad

type Except e = ExceptT e Identity

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
    fmap f (ExceptT ma) = ExceptT (fmap (fmap f) ma)

instance Monad m => Applicative (ExceptT e m) where
    pure a = ExceptT (pure $ Right a)
    ExceptT mf <*> ExceptT ma = ExceptT $ do
        f <- mf
        case f of
            Left err -> return $ Left err
            Right f' -> do 
                a <- ma
                case a of
                    Left err -> return $ Left err
                    Right a' -> return $ Right (f' a')


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

throwError :: Monad m => e -> ExceptT e m a
throwError = ExceptT . return . Left 

catchError :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
catchError m handler = ExceptT $ do
    a <- runExceptT m
    case a of
        Right a' -> return $ Right a'
        Left e -> runExceptT $ handler e