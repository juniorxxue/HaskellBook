{-# LANGUAGE FlexibleInstances #-}
module Functor where

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
