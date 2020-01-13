module Ex5 where

    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    fibs :: [Integer]
    fibs = [ fib x | x <- [0..]]

    fibs' :: [Integer]
    fibs' = 0 : 1 : [x + y | (x,y) <- zip fibs' (tail fibs') ]

    -- 0 1 1 2 3 5 8 13 

    main = do
        putStrLn (show (take 8 fibs))
        putStrLn (show (take 8 fibs'))