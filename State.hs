module State where

reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount list =
    (funcCount + 1, reverse list)

appendReversedWithCount :: Int -> [a] -> [a] -> (Int, [a])
appendReversedWithCount funcCount list1 list2 =
    let (funcCount', revList1) = reverseWithCount funcCount list1
      (funcCount'', revList2) = reverseWithCount funcCount' list2
    in (funcCount'' + 1, revList1 ++ revList2)


append3ReversedWithCount :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount funcCount list1 list2 list3 =
    let (funcCount1, revList1) = reverseWithCount funcCount list1
        (funcCount2, revList2) = reverseWithCount funcCount1 list2
        (funcCount3, revList3) = reverseWithCount funcCount2 list3
    in (funcCount3 + 1, revList1 ++ revList2 ++ revList3)

-- data State s a = State { runState :: s -> (s, a) }

-- return :: Monad m => a -> m a
-- (>>=)  :: Monad m => m a -> (a -> m b) -> m b

-- Found I lack expertise in Monad, go back to learn again