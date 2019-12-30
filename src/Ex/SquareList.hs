module SquareList where
    
    square [] = []
    square (x:xs) = [x*x] ++ square xs

    squareList n = [sum (square [1..x]) | x <- [1..n]]