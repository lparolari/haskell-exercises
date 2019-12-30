module Grid where
    grid m n = concat [[(x,y) | y <- [1..n] ] | x <- [1..m] ]