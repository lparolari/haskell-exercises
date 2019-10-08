module Validate where 

-- credit card validation

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherInternal (reverse x) False)
doubleEveryOtherInternal [] _ = []
doubleEveryOtherInternal (x:xs) False = [x] ++ doubleEveryOtherInternal xs True
doubleEveryOtherInternal (x:xs) True = [x * 2] ++ doubleEveryOtherInternal xs False

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

test1 = validate 4012888888881881
test2 = validate 4012888888881882
