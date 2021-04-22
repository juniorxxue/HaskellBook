module Monads where
import Data.Complex

-- You Could Have Invented Monads

f :: Float -> Float
f x = x + 1

g :: Float -> Float
g x = x + 2

compo = f . g -- compo works like (+3)

-- well, we want more debug info
f' :: Float -> (Float, String)
f' x = ((f x), "f was called. ")


g' :: Float -> (Float, String)
g' x = ((g x), "g was called. ")

compo' x = let (y, s) = g' x
               (z, t) = f' y
           in (z, s ++ t)

-- debuggable version of compo

-- so we invented bind function <- higher-order function

-- bind f' :: (Float, String) -> (Float, String)
bind :: (Float -> (Float, String)) -> ((Float, String) -> (Float, String))
bind ff (gx, gs) = let (fx, fs) = ff gx in (fx, gs ++ fs)

compo'' = (bind f') . g'

-- indentity

unit x = (x, "")

lift ff x = (ff x, "")

assertion1 = (bind (lift f) . (lift g) $ 1) ==
             (lift (f . g) $ 1)

-- bind and unit! to compose debuggable functions

myPlus :: (Num a) => a -> a -> a
myPlus x y = x + y
