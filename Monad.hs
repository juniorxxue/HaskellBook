module Monad where

-- List Monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

doNothing xs = xs >>= \x -> [x]

-- damn, no magic for List monad since list comprehension is primitive