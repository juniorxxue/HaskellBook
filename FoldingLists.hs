module FoldingLists where

-- foldr can be viewed by replacing the cons and nil
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (foldr f z xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

e1 = myAny even [1..]

xs = [1,2,3,4] ++ undefined
e2 = length (take 4 xs)

-- fold r requires head of list should be not undefined
-- fold l requires the provided const

conc = concat
f x y = conc ["(", x, "+", y, ")"]
e3 = foldl f "0" (map show [1..5])
e4 = scanr f "0" (map show [1..5])

-- both folds traverse the spine in the same direction.
-- What’s different is the associativity of the evaluation.

-- Right folds have to traverse the list outside-in
-- but the folding itself starts from the end of the list.

e5 = foldr (^) 2 [1..3]
-- foldr (^) 2 [1..3]
-- (1 ^ (2 ^ (3 ^ 2)))
-- 1

e6 = foldr (:) [] [1..3]
e7 = foldl (flip (:)) [] [1..3]

e8 = foldr (\a b -> undefined) [] ["Pizza", "Apple", "Banan"]
