-- foldr' g v xs
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' g v (x:xs) = g x (foldr' g v xs)
