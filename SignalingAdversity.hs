module SignalingAdversity where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
    Just $ Person name age
  | otherwise = Nothing

ex1 = mkPerson "John Browning" 160

-- smart constructor

-- usually we use Left to represent edge cases/error

-- Lifted and unlifted type
