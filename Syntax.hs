x :: ()
x =
  ()

y :: Int
y =
  0

f :: Int -> Int
f a = a

g :: Int
g = f y

data B = T | F
  deriving Show

h :: B
h = T

i :: B -> Int
i T = 1
i F = 0 -- this is zero

-- i = \b -> case b of
--   T -> 1
--   F -> 0

j = "hello"

class ToString a where
  toString :: a -> String

instance ToString B where
  -- toString = \b -> case b of
  --   T -> "T"
  --   F -> "F"
  toString T = "T"
  toString F = "F"

instance ToString () where
  toString = show

instance ToString B where
  toString _ = "B"
