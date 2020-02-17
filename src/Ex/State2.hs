module State where
    type State = Int
    newtype ST a = S (State -> (a, State))

    app :: ST a -> (State -> (a, State))
    app (S st) = st

    instance Monad ST where
        return x = S (\s -> (x, s))
        -- m >>= g :: f a -> (a -> f b) -> f b
        m >>= g = (S( \s -> let (v1,s') = (app m) s
                                (v2,s'') = app (g v1) s'
                        in (v2,s'')))

    instance Functor ST where
        -- fmap :: (a -> b) -> f a -> f b
        fmap g st = do
            x <- st
            return $ g x

    instance Applicative ST where
        -- pure :: a -> f a
        pure x = return x

        -- <*> :: f (a -> b) -> f a -> f b
        g <*> st = do
            f <- g
            x <- st
            return $ f x    
    
    data Tree a = Leaf a | Node (Tree a) (Tree a)

    view :: Tree Int -> String
    view (Leaf n) = show n
    view (Node l r) = (view l) ++ " " ++ (view r)

    fresh = S (\n -> (n, n+1))

    mlabel :: Tree a -> ST (Tree Int)
    mlabel (Leaf a) = do n <- fresh
                         return (Leaf n)
    mlabel (Node l r) = do l' <- mlabel l
                           r' <- mlabel r
                           return (Node l' r')
    
    main = do
        let t1 = Node (Node (Leaf 5) (Leaf 6)) (Leaf 18)
        putStrLn (view t1)
        let (v, s) = ((app (mlabel t1)) 0)
        putStrLn (view v)

