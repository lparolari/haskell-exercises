module Prime where

factors n = [x | x <- [1..n], n `mod` x == 0]

-- a number is prime if it has 2 divisors.
prime n | length (factors n) == 2 = True
        | otherwise = False

-- a number N is prime if its divisors are 1 and N.
prime' n = factors n == [1,n]