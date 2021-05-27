module MonoidSemigroup where

-- an algebra refers to some operations and the set they operate over

-- a monoid is a binary associative operation with an identity

-- type Monoid :: * -> Constraint
-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   {-# MINIMAL mempty #-}

-- mconcat = foldr mappend mempty

import Control.Monad
import Data.Monoid
import Test.QuickCheck

ass1 = (Sum 1) <> (Sum 2 <> Sum 3) == (Sum 1 <> Sum 2) <> Sum 3

-- Num has two monoids: Product and Sum
-- Bool has two monoids: Any and All
-- Maybe has two monoids: First and Last
expr1 = Last (Just 1) <> Last (Just 2) <> Last (Just 3)
expr2 = Last (Just 1) `mappend` Last (Just 2) `mappend` Last (Just 3)

-- distributed system use commutative monoids in designing and thinking about constraints
-- which are monoids that guarantee their operation commutes

-- indentity turns the operation into the indentity operation

-- Semigroup is monoid without a identity
-- we're pull legs from insects like creepy kids do

data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)

-- when haskller refer to algebras, they're usually talking about a somewhat informal notion of
-- operations over types and its laws

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

runQC1 = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

runQC2 = quickCheck (monoidLeftIdentity :: String -> Bool)
runQC3 = quickCheck (monoidRightIdentity :: String -> Bool)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

-- Exercise 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Exercise 2

newtype Identity a = Identity a

instance (Eq a) => Eq (Identity a) where
  (==) (Identity a) (Identity b) = (==) a b

instance (Show a) => Show (Identity a) where
  show (Identity a) = show a

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

-- Exercise 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool

-- Exercise 6

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return $ BoolConj True)
                        , (1, return $ BoolConj False) ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- Exercise 8

data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

-- Test

mainExer :: IO ()
mainExer = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)