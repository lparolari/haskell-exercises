module Lc where
    -- [f x | x <- xs, p x]
    lc f p xs = map f (filter p xs)

    test = lc (+5) (<4) [1,2,3,4,5,6]
    -- [6,7,8]