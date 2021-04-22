module Applicative where

import Control.Applicative

-- I got the motivation
demo = fmap (*) $ Just 3
-- which creates Just (3*)
-- to use it like a normal function, we need to pattern match on Just and extract it from
-- we need a general way to do


-- laws

-- identity: pure id <*> v = v
demo1 = pure id <*> [1..5]

-- composition

-- homomorphism
