module Relabel where

    type State = Int

    newtype ST a = S (State -> (a, State))

    app :: ST a -> State -> (a, State)
    app (S st) x = st x

    instance Monad ST where
        -- return :: a -> ST a
        return x = S (\s -> (x, s))
        -- (>>=) :: ST a -> (a -> ST b) -> ST b
        stx >>= f = S (\s ->
            let (x, s') = app stx s
            in app (f x) s')

    instance Functor ST where
        -- fmap :: (a -> b) -> ST a -> ST b
        fmap f st = do
            x <- st
            return $ f x
    
    instance Applicative ST where
        -- pure :: a -> ST a
        pure x = return x
        -- (<*>) :: ST (a -> b) -> ST a -> ST b
        stf <*> st = do
            f <- stf
            fmap f st

    data Tree a = Leaf a | Node (Tree a) a (Tree a)
                deriving Show

    -- state transformer that returns the current state as
    -- its result, and the next integer as the new state.
    fresh :: ST Int
    fresh = S (\n -> (n, n+1))

    rlabel :: Tree a -> ST (Tree Int)

    rlabel (Leaf x) = do
        n <- fresh
        return $ Leaf n

    rlabel (Node l x r) = S(
        \s ->  
            let (l', s') = app (mlabel l) s
                x = s' + 1
                (r', s'') = app (mlabel r) x
            in (Node l' x r', s''))
