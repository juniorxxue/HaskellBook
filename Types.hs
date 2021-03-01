{-# LANGUAGE NoMonomorphismRestriction #-}
module Types where

-- so except for a sprinkling of syntactic sugar for things like numbers or functions
-- everything originates in a data constructor from some definition of a type

-- Haskell has improved on System F in some key ways, such as by allowing general recursion
-- nd the Hindley-Milner system to permit type inference


-- 1. Type systems in logic and mathematics have been designed to impose constraints that enforce correctness
-- 2. Good type systems can also enable compiler optimizations
-- 3. Types can also serve as documentation of your program


-- :type 13
-- 13 :: Num a => a
x = 13 :: Integer

-- each typeclass offers a standard set of functions that can be used across several concrete types

-- We say it’s constrained because we still don’t know the concrete type of a
-- but we do know it can only be one of the types that has the required typeclass instance

fifteen = 15
fifteenInt = fifteen :: Int
-- fifteenDouble = fifteen :: Double

--The tuple of constraints does represent a product, or conjunction, of constraints

-- not :: Bool -> Bool
-- length :: [a] -> Int
-- concat :: [[a]] -> [a]
-- head :: [a] -> a
-- (<) :: (Ord a) => a -> a -> Bool

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

-- The term sectioning specifically refers to partial application of infix operators
-- which has a special syntax and allows you to choose
-- whether the argument you’re partially applying the operator to is the first or second argument

f :: a -> a -> a -> a
f = undefined

xx :: Char
xx = undefined

-- :t f xx

ourId x = x

example = 1

-- With respect to Haskell, the principal type is the most generic type which still typechecks.
