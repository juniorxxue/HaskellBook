module GADTs where

data Term t = Zero
            | Succ (Term Int)
            | Pred (Term Int)
            | IsZero (Term Int)
            | If (Term Bool) (Term t) (Term t)