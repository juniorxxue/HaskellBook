{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Functor where

import Test.QuickCheck
import Test.QuickCheck.Function

-- abstract out a common pattern
-- make certain it follows some laws
-- give it a awesome name

class Sumthin a where
  s :: a -> a


data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

-- that's the main motivation
-- to further generalize treeMap

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- functor laws
-- 1. id: fmap id = id
-- 2. compositional: fmap (g. f) = fmap g . fmap f

getInt :: IO Int
getInt = fmap read getLine

-- functor is unique
data Tuple a b =
  Tuple a b
  deriving (Eq, Show)

newtype Flip f a b =
  Flip (f b a) deriving (Show, Eq)

data FixMePls a = FixMe
                | Pls a
                deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- Exercise 16.10

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool

-- 2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

type PrFC = Pair Int -> IntToInt -> IntToInt -> Bool

--  Lifted Functions

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- main

main :: IO ()
main = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorCompose' :: IdFC)
  quickCheck (functorCompose' :: PrFC)