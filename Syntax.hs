import Prelude
import Data.String

x :: String
x = "Hello"

y :: Int
y = 2

z :: Bool
z = True

w :: [Int]
w = [1, 2, 3]

f :: Int -> Int
f a = a + 1

g :: Int -> Int -> Int
g a b = a + b

h :: Int
h = 1 `g` 2

-- <A> A i(A x)
i :: a -> a
i x = x

data Person = MkPerson String Int

brian :: Person
brian = MkPerson "Brian" 27

data Z = X String | Y
  deriving (Show)

instance Eq Z where
  X a == X b = a == b
  Y == Y = True
  _ == _ = False

  -- a /= b = not (a == b)

class ToString a where
  toString :: a -> String

instance ToString Int where
  toString = show

instance ToString Bool where
  toString True = "True"
  toString False = "False"

j :: Z
j = X "Testing"

k :: Z -> String
k (X s) = s
k Y = "y"

l :: Z -> String
l z = case z of
  X s -> s
  Y -> "y"

