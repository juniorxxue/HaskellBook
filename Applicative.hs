module Applicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools)
                          ,(1, return Twoo)]


instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools

instance EqProp Bull where (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two a1 f) (Two a2 x) = Two (mappend a1 a2) (f x)

main :: IO ()
main = do
    quickBatch (monoid Twoo)