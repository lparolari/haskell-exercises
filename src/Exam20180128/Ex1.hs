module Ex1 where
    reverse' :: [a] -> [a]
    reverse' xs = foldr (\x y -> y ++ [x]) [] xs

    reverse'' :: [a] -> [a]
    reverse'' xs = foldl (\v x -> x : v) [] xs 