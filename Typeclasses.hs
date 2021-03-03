module Typeclasses where

-- class MyEq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- instance MyEq Int
-- instance MyEq Bool

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Show)

data Date =
  Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayofMonth) (Date weekday' dayofMonth') =
    weekday == weekday' && dayofMonth == dayofMonth'

data Identity a =
  Identity a

-- Exercises Eq Instances

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn x' =
    x == x'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two x' y' =
    x == x' && y == y'

data StringOrInt = TisAnInt Int
                 | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt x' = x == x'
  TisAString x == TisAString x' = x == x'

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  Pair a1 a2 == Pair a1' a2' =
    a1 == a1' && a2 == a2'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' =
    a == a' && b == b'

data Which a = ThisOne a
             | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne a == ThisOne a' = a == a'
  ThatOne a == ThatOne a' = a == a'

data EitherOr a b = Hello a b
                  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a b == Hello a' b' = a == a' && b == b'
  Goodbye b == Goodbye b' = b == b'

-- default types of typeclasses are suber coool

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

-- typeclasses are dispatched by type

-- 1. a typeclass defines a set of functions and/or values;
-- 2. types have instances of that typeclass;
-- 3. the instances specify the ways that type uses the functions of the typeclass.

-- how does Haskell know where to find code?

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)


instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 100
