module Ex4 where

    type State = String
    newtype Stack a = Stack (State -> [(a, State)])

    -- app :: Stack a -> State -> [(a, State)]
    app (Stack f) s = f s

    instance Functor Stack where
        -- fmap :: (a -> b) -> Stack a -> Stack b
        fmap f stk = Stack (\s -> let [(x, s')] = (app stk s) in [(f x , s')])

    instance Applicative Stack where
        -- pure :: a -> Stack a
        pure x = Stack (\s -> [(x, s)])

        -- <*> :: Stack (a -> b) -> Stack a -> Stack b
        fa <*> xa =
            Stack (\s -> 
                let [(f, s')] = app fa s
                    [(x, s'')] = app xa s'
                in 
                    [(f x, s'')])

    instance Monad Stack where
        -- >>= :: Stack a -> (a -> Stack b) -> Stack b
        xm >>= f = Stack (\s ->
            let [(x, s')] = app xm s
                [(y, s'')] = app (f x) s'
            in 
                [(y, s'')])

    -- pop :: Stack Char
    pop = Stack (\state ->
        case state of
            [] -> []
            s:sx -> [(s, sx)])

    -- push :: Char -> Stack ()
    push c = Stack (\s -> [((), c:s)])

    -- balance :: String -> Pila Bool
    balance [] = Stack (\s -> case s of
        [] -> [(True, s)]
        _ -> [(False, s)])
    balance ('(':xs) = do
        push '('
        balance xs
    balance (')':xs) = do
        pop
        balance xs
    balance (_:xs) = do
        balance xs

    main = do
        let [(r1,s1)] = app (balance "a (b) (c)") "" 
        let [(r2,s2)] = app (balance "a ((b) + (c*2)") ""
        putStrLn ("Test 1: " ++ show r1 ++ " should be True, " ++ s1)
        putStrLn ("Test 2: " ++ show r2 ++ " should be False, " ++ s2)