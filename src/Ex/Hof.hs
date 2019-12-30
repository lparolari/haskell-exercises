module Hof where

    and' xs = foldr (&&) True xs

    all' :: (a -> Bool) -> [a] -> Bool
    all' p xs = and (map p xs)

    testAll = all' (==True) [True, True, True]

    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    map' f = foldr (\x xs -> (f x) : xs) []
    
    -- filter!

    -- dec2int [2,3,4,5] -> 2345
    dec2int xs = foldl (\x -> \y -> 10*x + y) 0 xs


    newtype ZipList a = Z [a] deriving Show

    instance Functor ZipList where
        fmap f (Z xs) = Z (map f xs)

    instance Applicative ZipList where
        pure x = Z (repeat x)
        (Z fz) <*> (Z xs) = Z [g x | (g,x) <- zip fz xs]