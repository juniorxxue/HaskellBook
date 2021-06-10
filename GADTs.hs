{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module GADTs where

-- data Exp = ValInt Int
--          | ValBool Bool
--          | Add Exp Exp
--          | Equa Exp Exp
--          deriving (Eq, Show)

-- data Exp a = ValInt Int
--            | ValBool Bool
--            | Add (Exp a) (Exp a)
--            | Equa (Exp a) (Exp a)
--            deriving (Eq, Show)

data Exp a where
    ValInt :: Int -> Exp Int
    ValBool :: Bool -> Exp Bool
    Add :: Exp Int -> Exp Int -> Exp Int
    Equa :: Exp Int -> Exp Int -> Exp Bool


class Test a where
    testFun :: Int -> a Int