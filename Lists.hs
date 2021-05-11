module Lists where

myHead (x : _) = x

-- [1, 3, 4]
-- syntactic suger for (1 : 2 : 3 : [])

myTail :: [a] -> [a]
myTail [] = []
myTail (_ : xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

-- [1..10]
-- enumFromThenTo 1 2 10

ex1 = takeWhile (>6) [1..10]