module Monad where

-- fmap f xs = xs >>= return . f

import Control.Monad (join)

-- concat :: [[a]] -> [a]

-- join :: Monad m => m (m a) -> m a
-- fmap :: Monad m => (a -> b) -> m a -> m b

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- list Monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]


-- Maybe Monad
data Cow = Cow {
    name   :: String,
    age    :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c 
    in if n == "Bess" && w > 499
       then Nothing
       else Just c



mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agy ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty -> weightCheck (Cow nammy agy weighty)



mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)


-- -- right identity
-- m >>= return = m
-- -- left identity
-- return x >>= = fx
-- Basically both of these laws are saying that return should be neutral
-- and not perform any computation


data Nope a =
    NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither b a = Left' a
                         | Right' b

instance Functor (PhhhbbtttEither b) where
    fmap f (Left' x) = Left' (f x)
    fmap f (Right' y) = Right' y

instance Applicative (PhhhbbtttEither b) where
    pure x = Left' x
    (<*>) (Right' y) _ = Right' y
    (<*>) _ (Right' y) = Right' y
    (<*>) (Left' f) (Left' x) = Left' (f x)

instance Monad (PhhhbbtttEither b) where
    return = pure
    (>>=) (Right' y) _ = Right' y
    (>>=) (Left' x) f = f x

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)