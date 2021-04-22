module SignalingAdversity where
import Data.Maybe

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

-- https://wiki.haskell.org/Smart_constructors
-- runtime checking is smart constructor
-- type system checking is

-- usually we use Left to represent edge cases/error
-- Either type contains more information than Maybe type
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)
-- Case expressions and pattern matching will work without an Eq instance, but guards using (==) will not.
mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

-- Type constructors (that is, higher-kinded types) are types that take more types as arguments.

-- Lifted and unlifted type

-- Unlifted types are any type which cannot be inhabited by bottom.

-- Exercises
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

countTheBeforeVowel :: String -> String
countTheBeforeVowel = undefined

-- vowel: a e i o u
