module Qsort where

    qsort [] = []
    qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]
    
    test = qsort [2,2,3,1,1]
    