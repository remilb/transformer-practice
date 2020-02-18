module LiftSigs where

-- * Signatures for specialized lifts

type Catch e m a = m a -> (e -> m a) -> m a

type Listen w m a = m a -> m (w, a)

type Pass w m a = m (w -> w, a) -> m a
