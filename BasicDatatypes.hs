module BasicDatatypes where

-- data constructors
-- its called a data declaration
-- data Bool = False | True
-- sum type

data Mood = Blah | Woot deriving Show

-- Mood
-- Blash or Woot

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

type Name = String
data Pet = Cat | Dog Name

-- Data declarations always create a new type constructor,
-- but may or may not create new data constructors.

-- Polymorphism in Haskell is either parametric or constrained.
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y

-- Is polymorphic, but constrained or bounded
-- to the set of types which have an instance of the Eq typeclass

-- Conventions for variables
-- type variables : a, b, c
-- function arguments : f, f1, g, g1
-- xs is the plural of x
