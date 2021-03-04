module MoreFunctionalPatterns where

myNum :: Integer
myNum = 1

myVal f = myNum

mTh :: Num a => a -> a -> a -> a
mTh x y z = x

exer1 = mTh 3

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (+1)

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

-- named entities and anonymous entities evaluate differently in Haskell

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered"
printUser (RegisteredUser (Username name) (AccountNumber n)) = putStrLn $ name ++ " " ++ show n

k :: (a, b) -> a
k (x, y) = x

functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0


-- guards reply on truth values to decide between two or more possible results

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = -x
  | otherwise = x

-- pointfree style

myprint :: Show a => a -> IO ()
myprint = (putStrLn . show)
