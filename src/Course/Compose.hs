{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))
  deriving Show

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g a = f (g a)

-- Optional (List Int)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose fga) =
    Compose ((f <$>) <$> fga)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure :: a -> Compose f g a
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: 
    Compose f g (a -> b) 
    -> Compose f g a
    -> Compose f g b
  (<*>) (Compose fgab) (Compose fga) =
    Compose (lift2 (<*>) fgab fga)

getCompose :: Compose f g a -> f (g a)
getCompose (Compose fga) =
  fga

-- data Compose f g a = Compose (f (g a))
-- data OptionalT f a = OptionalT (f (Optional a))
-- data ListT f a = ListT (f (List a))
