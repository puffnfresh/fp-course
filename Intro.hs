


import Prelude
import Data.String

a :: Int
a = 1

b :: Bool
b = True

c :: Char
c = 't'

f :: Int -> Int
f a = a + 1
-- f = \a -> (a + 1)

g :: Int -> (Int -> Int)
g = \a -> (\b -> a + b + 1)
-- g a b = a + b + 1

plus :: Int -> Int -> Int
plus a b = a + b

-- > g 10 20
-- 31

-- function f(a) { return a; }
-- var f = function(a) { return a; }
-- \a -> a
-- function(a) { return a; }

-- var g = (a) => (b) => a + b + 1;







identity :: a -> a
identity a = a


constant :: a -> b -> a
constant a _ = a


data YesNo = Yes | No deriving Show

toInt :: YesNo -> Int
toInt x =
  case x of
    Yes -> 1
    No -> 0

-- toInt Yes = 1
-- toInt No = 0


flip' :: YesNo -> YesNo
flip' Yes = No
flip' No = Yes

