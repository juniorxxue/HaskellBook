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


import Data.Monoid

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

data Test =
  String :<><>: Int

-- cool
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = undefined

-- when haskller refer to algebras, they're usually talking about a somewhat informal notion of
-- operations over types and its laws
