{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Compose (Compose(..)) where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

import Course.Optional

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) ::
    (a -> b)
    -> Compose f g a
    -> Compose f g b
  (<$>) f (Compose fga) =
    Compose ((f <$>) <$> fga)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) ::
    Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b
  (<*>) (Compose fgab) (Compose fga) =
    Compose (lift2 (<*>) fgab fga)

-- instance (Monad f, Monad g) =>
--   Monad (Compose f g) where
-- -- Implement the (=<<) function for a Monad instance for Compose
--   (=<<) ::
--     (a -> Compose f g b)
--     -> Compose f g a
--     -> Compose f g b
--   (=<<) f (Compose fga) =
--     error "impossible"




-- List (IO (List (IO a)))

-- data OptionalT f a
--   = OptionalT (f (Optional a))

-- runOptionalT ::
--   OptionalT f a
--   -> f (Optional a)
-- runOptionalT (OptionalT fa) =
--   fa

-- instance Monad f => Monad (OptionalT f) where
--   (=<<) ::
--     (a -> OptionalT f b)
--     -> OptionalT f a
--     -> OptionalT f b
--   (=<<) f (OptionalT fa) =
--     OptionalT (fa >>= \oa -> (runOptionalT . f <$> oa) ?? pure Empty)
