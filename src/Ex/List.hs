module List where
    data List a = Nil | Cons a (List a)
        deriving (Show)

    toList :: [a] -> List a
    toList [] = Nil
    toList (x:xs) = Cons x (toList xs)

    toTheirList :: List a -> [a]
    toTheirList Nil = []
    toTheirList (Cons x xs) = [x] ++ toTheirList xs
