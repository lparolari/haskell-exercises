module Maybe where
    data Maybe a = Nothing' | Just' a
        deriving (Show)

    instance Functor Maybe.Maybe where
        -- fmap :: (a -> b) -> f a -> f b
        fmap _ Nothing' = Nothing'
        fmap g (Just' x) = Just' (g x)

    ex1 = fmap (+1) (Just' 1)

    instance Applicative Maybe.Maybe where
        -- pure :: a -> f a
        -- <*> :: f (a -> b) -> f a -> f b
        pure x = Just' x
        Nothing' <*> _ = Nothing'
        (Just' g) <*> mx = fmap g mx
    
    ex2 = pure (+) <*> (Just' 5) <*> (Just' 6)
    ex2a = (+) <$> (Just' 5) <*> (Just' 6)

    instance Monad Maybe.Maybe where
        -- m >>= f :: f a -> (a -> f b) -> f b
        m >>= f = case m of
            Nothing' -> Nothing'
            (Just' x) -> f x
    
    safediv _ 0 = Nothing'
    safediv n m = Just' (n `div` m)

    ex3 = pure 10 >>= \m ->
          pure 5 >>= \n ->
          safediv m n
    ex3a = do m <- pure 10
              n <- pure 5
              safediv m n