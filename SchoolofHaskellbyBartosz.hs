module SchoolofHaskellbyBartosz where

-- putStrLn' str = do
--   putStr str
--   putChar '\n'

-- putQStrLn str = do
--   putChar '"'
--   putStr str
--   putChar '"'
--   putChar '\n'

-- main = do
--   putStrLn' "First Line"
--   putStrLn' "Second Line"
--   putQStrLn "You can quote me"
--   str <- getLine
--   putQStrLn str

data Token
data Expression

tokenize :: String -> [Token]
tokenize = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = do
  line <- getLine
  putStrLn line
  main
