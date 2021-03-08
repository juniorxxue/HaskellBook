module Lists where

myHead (x : _) = x

-- [1, 3, 4]
-- syntactic suger for (1 : 2 : 3 : [])

-- [1..10]
-- enumFromThenTo 1 2 10

ex1 = takeWhile (>6) [1..10]
