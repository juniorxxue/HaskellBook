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