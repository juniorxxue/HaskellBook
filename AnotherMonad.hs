{-# LANGUAGE GADTSyntax #-}

module AnotherMonad where

data Exp where
    Lit :: Integer -> Exp
    Add :: Exp -> Exp -> Exp
    Sub :: Exp -> Exp -> Exp
    Mul :: Exp -> Exp -> Exp
    Div :: Exp -> Exp -> Exp

eval :: Exp -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

safeEval :: Exp -> Maybe Integer
safeEval (Lit n) = Just n
safeEval (Add e1 e2) =
    case safeEval e1 of
        Nothing -> Nothing
        Just n1 -> case safeEval e2 of
            Nothing -> Nothing
            Just n2 -> Just (n1 + n2)
safeEval (Sub e1 e2) =
    case safeEval e1 of
        Nothing -> Nothing
        Just n1 -> case safeEval e2 of
            Nothing -> Nothing
            Just n2 -> Just (n1 - n2)
safeEval (Mul e1 e2) =
    case safeEval e1 of
        Nothing -> Nothing
        Just n1 -> case safeEval e2 of
            Nothing -> Nothing
            Just n2 -> Just (n1 - n2)
safeEval (Div e1 e2) =
    case safeEval e1 of
        Nothing -> Nothing
        Just n1 -> case safeEval e2 of
            Nothing -> Nothing
            Just n2 -> if n2 == 0 then Nothing else Just (n1 `div` n2)


evalSeq :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
evalSeq mi f = case mi of
    Nothing -> Nothing
    Just a -> f a


betterEval :: Exp -> Maybe Integer
betterEval (Lit n) = Just n
betterEval (Add e1 e2) =
    betterEval e1 `evalSeq` \n1 ->
    betterEval e2 `evalSeq` \n2 ->
    Just (n1 + n2)
betterEval (Sub e1 e2) =
    betterEval e1 `evalSeq` \n1 ->
    betterEval e2 `evalSeq` \n2 ->
    Just (n1 - n2)
betterEval (Mul e1 e2) =
    betterEval e1 `evalSeq` \n1 ->
    betterEval e2 `evalSeq` \n2 ->
    Just (n1 * n2)
betterEval (Div e1 e2) =
    betterEval e1 `evalSeq` \n1 ->
    betterEval e2 `evalSeq` \n2 ->
    if n2 == 0 then Nothing else Just (n1*n2)