module Unfold where

    -- Given the definition of unfold redefine
    -- * chop2 (8)
    -- * map f
    -- * iterate f

    unfold p h t x | p x = [] 
                   | otherwise = h x : unfold p h t (t x)

    chop2 = unfold (== []) (take 2) (drop 2)

    map' f = unfold (==[]) (f.head) (tail)
