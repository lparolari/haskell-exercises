-- foldl' v g xs
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' g v (x:xs) = foldl' g (g v x) xs 