module Tree1 where

    data Tree a = Leaf | Node (Tree a) a (Tree a)

    instance Show a => Show (Tree a) where
        show Leaf = ""
        show (Node l x r) =  (show x) ++ " " ++ show l ++ "" ++ show r

    -- Define the followig functions
    --  * repeat
    --  * take
    --  * replicate

    -- repeat' :: a -> Tree a
    repeat' x = Node (repeat' x) x (repeat' x)

    -- take' :: Int -> Tree a -> Tree a
    take' 0 _ = Leaf
    take' _ Leaf = Leaf
    take' n (Node l x r) = Node (take' (n-1) l) x (take' (n-1) r)

    -- replicate' :: Int -> a -> Tree a
    replicate' n x = take' n (repeat' x)

    -- ***
    
    main = do
        putStrLn $ show $ replicate' 2 1