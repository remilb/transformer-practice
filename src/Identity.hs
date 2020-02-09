module Identity where

import           Data.Functor
import           Control.Applicative
import           Control.Monad

-- | The identity monad
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a
