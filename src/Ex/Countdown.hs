module Countdown where
    data Op = Add | Sub | Mul | Div

    instance Show Op where
        show Add = "+"
        show Sub = "-"
        show Mul = "*"
        show Div = "/"
    
    valid :: Op -> Int -> Int -> Bool
    valid op x y = case op of
        Add -> True
        Sub -> x - y > 0
        Mul -> True
        Div -> x `mod` y == 0
    
    apply :: Op -> Int -> Int -> Int
    apply op x y = case op of
        Add -> x + y
        Sub -> x - y
        Mul -> x * y
        Div -> x `div` y

    data Expr = Val Int | App Op Expr Expr

    instance Show Expr where
        show (Val n) = show n
        show (App o x y) = brak x ++ show o ++ brak y where
            brak (Val n) = show n
            brak a = "(" ++ show a ++ ")"
    
    values :: Expr -> [Int]
    values e = case e of
        Val n -> [n]
        App o x y -> values x ++ values y
    
    eval :: Expr -> [Int]
    eval e = case e of
        Val n -> [n | n > 0]
        App o l r -> [ apply o x y | x <- eval l, y <- eval r, valid o x y]
    
    -- all subsequences from a list
    subs :: [a] -> [[a]]
    subs [] = [[]]
    subs (x:xs) = yss ++ map (x:) yss
        where yss = subs xs
    -- or
    -- subs = filterM (\_ -> [True, Flase]) 
    -- subs (x:xs) = filterM (\_ -> [True, Flase]) (x:xs)

    interleave :: a -> [a] -> [[a]]
    interleave n [] = [[n]]
    interleave n (x:xs) = (n:x:xs) : map (x:) (interleave n xs)

    perms :: [a] -> [[a]]
    perms [] = [[]]
    perms (x:xs) = concat (map (interleave x) (perms xs))

    choices :: [a] -> [[a]]
    choices = concat . map perms . subs

    solution :: Expr -> [Int] -> Int -> Bool
    solution e ns n = elem (values e ) (choices ns) && eval e == [n]

    test = let e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)) in solution e [1,3,7,10,25,50] 765