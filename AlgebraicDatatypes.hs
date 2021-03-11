module AlgebraticDatatypes where

-- why call it algebratic

-- haskell offers sum types, product types, products with records

-- type constructors & data constructors

-- Bool is a type constant, it enumerates two values that are also constants

-- Be careful with the box/container analogy as it will betray you later
-- not all type arguments to constructors have value-level witnesses! Some are phantom.

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

myOtherHusky :: HuskyType [[[Int]]]
myOtherHusky = HuskyData

data Doggies a = Husky a
               | Mastiff a
               deriving (Eq, Show)

-- So the behavior of constructors is such that if they don’t take any arguments
-- they behave like (type or value-level) constants
-- If they do take arguments
-- they act like (type or value-level) functions that don’t do anything except get applied

-- Information about types does not persist through to runtime

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapulatsR'Us
             | TakeYourChanceUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- myCar : Vehicle

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- areCars [myCar, clownCar, urCar]

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

-- arity : nullary, unary, binary


-- Algebraic datatypes in Haskell are algebraic
-- because we can describe the patterns of argument structures using two basic operations
-- sum and product

-- cardinality of a datatype

-- record or tagged union (product sum type)

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


-- newtype can be erased at runtime

-- Think of an algebraic data type as a type composed of simpler types

-- tagged union types is sum type
-- untagged union type is union type <- typed racket

-- a product is like a struct, brilliant!!!!

type TwoQs = (Bool, Bool)

data Person =
  Person { name :: String
         , age :: Int}
  deriving (Eq, Show)

-- wow, distribuity
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

-- sum of products is a normal form


data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- it's a normal form


-- Gardener * (Gardenia + Dailsy + Rose + Lilac)

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show

-- Awesome, function type is exponential
data List a = Nil
            | Cons a (List a)
