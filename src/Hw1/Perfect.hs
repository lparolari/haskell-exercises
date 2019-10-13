module Perfect where

-- A positive integer is perfect if it equals the sum of its factors
-- (excluding the number self), define perfects :: Int ->[Int] that,
-- given n, computes the list of all perfect numbers in [1..n]

factors n = [x | x <- [1..n], n `mod` x == 0]
factors' n = take (length (factors n) - 1) (factors n)

perfect n = (foldl (+) 0 (factors' n)) == n

perfects n = [x | x <- [1..n], perfect x]