module LiftSigs where

-- * Signatures for specialized lifts

type Catch e m a = m a -> (e -> m a) -> m a
