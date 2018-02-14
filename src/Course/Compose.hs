{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))
  deriving Show

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose fga) =
    -- Compose ((\ga -> f <$> ga) <$> fga)

    Compose (((<$>) . (<$>)) f fga)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where

  pure :: a -> Compose f g a
  pure =
    Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- Compose f <*> Compose fga =
  (<*>) (Compose f) (Compose fga) =
    Compose (lift2 (<*>) f fga)

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) f (Compose fga) =
    error "test"
