module Ex1 where

    -- instance Functor ((->) a) where
    --     -- fmap :: (a -> b) -> f a -> f b
    --     -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    --     fmap = (.)

    -- instance Applicative ((->) a) where
    --     -- pure :: a -> f a
    --     -- pure :: b -> a -> b
    --     pure y = \x -> y

    --     -- <*> :: f (a -> b) -> f a -> f b
    --     -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
    --     f <*> g = \x -> (f x (g x))

    instance Monad ((->) a) where
        -- pure :: a -> f a
        -- pure :: b -> a -> b
        pure y = \x -> y

        -- >>= :: f a -> (a -> f b) -> f b
        -- >>= :: (a -> b) -> (b -> a -> c) -> (a -> c)
        mx >>= f = \x -> (f (mx x) x)
