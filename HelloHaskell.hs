module HelloHaskell where

triple x = x * 3

area x = 3.14 * x * x

double x = x * 2

x = 7
y = 10
f = x + y

exp1 = (2^) $ (*30) $ 2 + 2

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

ex2 = let x = 5; y = 6 in x * y

mult1 = x * y
  where x = 5
        y = 6
