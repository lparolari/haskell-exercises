module Validate where 

-- Credit card validation.

-- Convert an integer to a list of numbers between 0 and 9 representing
-- the digits of the number. Zero and negative numbers are ignored returning
-- the empty list.
toDigits :: Integer -> [Integer]
toDigits x | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
           | otherwise = []
toDigitsRev = reverse . toDigits

-- Double every even digit starting from the right.
-- Digits are 1 indexed, so the first digit is not double, the second is, and so on. 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [doubleIfEven x y | (x,y) <- zip (reverse xs) [1..]]

doubleIfEven :: Integer -> Integer -> Integer
doubleIfEven x y = if y `mod` 2 == 0 then x * 2 else x

-- Sum all digits from all numbers in list. 
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

-- Perform the validation.
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

-- Main.
myMain = do
    putStrLn $ "The credit card 4012888888881881 is " ++ if (validate 4012888888881881) then "valid" else "NOT valid"
    putStrLn $ "The credit card 4012888888881882 is " ++ if (validate 4012888888881882) then "valid" else "NOT valid"