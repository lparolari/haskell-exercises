module MergeOrdered where

    merge [] rs = rs
    merge ls [] = ls
    merge (l:ls) (r:rs) = 
        if l <= r 
            then [l] ++ merge ls (r:rs) 
            else [r] ++ merge (l:ls) rs

    test = merge [2,5,6] [1,3,4]