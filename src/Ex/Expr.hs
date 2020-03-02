module Expr where
    
    data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

    instance Functor Expr where
        fmap f (Var x) = (Var (f x))
        fmap f (Val n) = (Val n)
        fmap f (Add l r) = let l' = fmap f l
                               r' = fmap f r
                           in Add l' r'

    instance Applicative Expr where
        pure x = (Var x)
        (Var f) <*> fx = fmap f fx
        (Val n) <*> fx = Val n
        (Add l r) <*> fx = Add (l <*> fx) (r <*> fx)

    instance Monad Expr where
        (Var x) >>= f = f x
        (Val n) >>= f = (Val n)
        (Add l r) >>= f = Add (l >>= f) (r >>= f)

    main = do
        putStrLn (show (fmap (\x -> 7) (Add (Var 'x') (Val 1))))
        putStrLn (show (pure (\x -> 7) <*> (Add (Var 'x') (Val 1))))
        putStrLn (show ((Var 'x') >>= (\x -> pure 7)))

    -- Exercise 3, test 23/01/2018
    -- Infer the types of the following function definition. 
    k (Add x y) g = Add (k x g) (k y g)