{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Prelude where

data Bool where
    True :: Bool
    False :: Bool

data Ordering where
    LT :: Ordering
    EQ :: Ordering
    GT :: Ordering

data Int where
    Zero :: Int
    Succ :: Int -> Int

data List a where
    Nil :: List a
    Cons :: a -> List a

data Maybe a where
    Nothing :: Maybe a
    Just :: a -> Maybe a

data Tree a where
    Leaf :: Tree a
    Node :: a -> (Tree a) -> (Tree a) -> Tree a

class Eq a where
    (==) :: a -> a -> Bool

class (Eq a) => Ord a where
    compare :: a -> a -> Ordering

class Bounded a where
    minBound :: a
    maxBound :: a

class Enum a where
    toNum :: Int -> a
    fromEnum :: a -> Int

class Functor f where
    fmap :: (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

($) :: (a -> b) -> a -> b
($) f x = f x

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f Nothing = Nothing
    fmap f (Just x) = Just $ f x

instance Bounded Bool where
    minBound :: Bool
    minBound = False

    maxBound :: Bool
    maxBound = True