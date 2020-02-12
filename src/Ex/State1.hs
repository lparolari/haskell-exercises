module State1 where
    type State = Int
    newtype ST a = S (State -> (a, State))

    app :: ST a -> (State -> (a, State))
    app (S st) = st

    instance Functor ST where
        -- fmap :: (a -> b) -> f a -> f b
        fmap g st = do x <- st
                       return (g x)
    
    simpleSt = pure 0
    ex1 = let x = simpleSt in (app x) 0
    ex2 = let x = (fmap (+1) simpleSt) in (app x) 0

    instance Applicative ST where
        -- pure
        -- <*> :: f (a -> b) -> f a -> f b

        pure x = (S (\s -> (x, s)))

        -- <*> :: S (State -> (a -> b, State)) -> S (State -> (a, State)) -> S (State -> (b, State))
        g <*> st = g >>= \f ->
                   st >>= \x ->
                   return (f x)    
    ex3 = app (pure (+) <*> pure 4 <*> pure 4) 7
    -- (8,7)
    
    instance Monad ST where
        -- m >>= g :: f a -> (a -> f b) -> f b
        -- m >>= g :: S (State -> (a, State)) -> (a -> S (State -> (b, State))) -> S (State -> (b, State))
        m >>= g = (S( \s -> let (v1,s') = (app m) s
                                (v2,s'') = app (g v1) s'
                        in (v2,s'')))
    
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
    
    ex5 n = let x = Node (Node (Leaf 5) (Leaf 6)) (Leaf 18)
                (v, s) = app (mlabel x) n in view v
