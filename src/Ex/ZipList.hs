module ZipList where
    
    newtype ZipList a = Z [a] deriving Show

    instance Functor ZipList where
        fmap g (Z xs) = Z [g x | x <- xs]
    
    instance Applicative ZipList where
        pure x = Z (repeat x)
        (Z gs) <*> (Z xs) = Z [f x | (f,x) <- zip gs xs]
    
    main = do
        let xs = pure 3
            res = 