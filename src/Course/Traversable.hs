{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne a) =
    ExactlyOne <$> f a

-- runExactlyOne

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse f (Full a) =
    Full <$> f a
  traverse _ Empty =
    pure Empty

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA =
  traverse id

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse ::
    Applicative h =>
    (a -> h b)
    -> Compose f g a
    -> h (Compose f g b)
  traverse f (Compose fga) =
    Compose <$> traverse (traverse f) fga

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)
  deriving Show

instance (Functor f, Functor g) =>
  Functor (Product f g) where
  (<$>) ::
    (a -> b)
    -> Product f g a
    -> Product f g b
  (<$>) f (Product fa ga) =
    Product (f <$> fa) (f <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
  traverse ::
    (Applicative h) =>
    (a -> h b)
    -> Product f g a
    -> h (Product f g b)
  traverse f (Product fa ga) =
    lift2 Product (traverse f fa) (traverse f ga)

-- (:.) :: a -> List a -> List a
-- lift2 (:.) :: f a -> f (List a) -> f (List a)

-- Product :: f a  ->    g a -> Product f g a
--         h (f a) -> h (g a) -> h (Product f g a)

-- lift2 Product


-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
  (<$>) ::
    (a -> b)
    -> Coproduct f g a 
    -> Coproduct f g b
  (<$>) f (InL fa) =
    InL (f <$> fa)
  (<$>) f (InR ga) =
    InR (f <$> ga)

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
  traverse ::
    (Applicative h) =>
    (a -> h b)
    -> Coproduct f g a
    -> h (Coproduct f g b)
  traverse f (InL fa) =
    InL <$> traverse f fa
  traverse f (InR ga) =
    InR <$> traverse f ga
