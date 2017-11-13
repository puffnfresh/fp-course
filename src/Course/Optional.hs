{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)


-- x :: Bool
-- x = True

-- f True = 0
-- f False = 1


-- g :: Bool -> (Int -> P.String)
-- g True i = show i
-- g False _ = "ignored"

-- ignored :: Int -> P.String
-- ignored = g False


-- g' :: Bool -> Int -> P.String
-- g' = \ b i -> 
--   case b of
--     True -> show i
--     False -> "ignored"

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b)
  -> Optional a
  -> Optional b
mapOptional f =
  bindOptional (Full . f)

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional f (Full a) =
  f a
bindOptional _ Empty =
  Empty

-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) ::
  Optional a
  -> a
  -> a
(??) (Full a) _ =
  a
(??) Empty a =
  a

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a
  -> Optional a
  -> Optional a
(<+>) Empty a =
  a
(<+>) a _ =
  a

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional (\a' -> f' a') a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
