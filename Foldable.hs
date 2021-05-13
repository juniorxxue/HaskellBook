module Foldable where

-- Folding necessarily implies a binary associative operation that has an identity value.

data Identity a =
    Identity a


instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x


data Constant a b =
    Constant b

instance Foldable (Constant a) where
    foldr f z (Constant x) = f x z
    foldl f z (Constant x) = f z x
    foldMap f (Constant x) = f x

data Two a b =
    Two a b


-- Follow resources
