module GADT where

-- GADTs for dummies :)

data Ethr a b = Lft a | Rgt b

-- type constructor: Either
-- data constructors: Left, Right

isLeft (Lft a) = True
isLeft (Rgt a) = False

type X a = Ethr a a

-- type function: X
-- type functions like X are very limited compared to ordinary functions,
-- no pattern matching, no multiple arguments, no recursion

type F [a] = Set a

-- error   Unexpected type ‘[a]’ ​parser
--                  In the type declaration for ‘F’
--                  A type declaration should have form
--                    type F a = ...

type F Bool = Char
     F String = Int

type F a
    | IsSimple a == TrueType = a

type IsSimple Bool = TrueType

data TrueType = T

-- type predicate!
class IsSimple a
instance IsSimple Bool

class HasInt a
instance HasInt Int
instance (HasInt a) => HasInt [a]
instance (HasInt a) => HasInt (Map a b)
instance (HasInt b) => HasInt (Map a b)

-- they're not selected in order, most specific is automatically selected