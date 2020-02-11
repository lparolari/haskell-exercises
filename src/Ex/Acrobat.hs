module Acrobat where

    type Pole = (Int, Int)
    data Line a = L(Pole -> (a, Pole))

    -- Note: the acrobat falls if the difference between birds on 
    --       left and right side of the pole is greather or equal
    --       to 4.

    -- app :: Line a -> Pole -> (a, Pole)
    app (L l) p = l p

    instance Functor Line where
        -- fmap :: (a -> b) -> f a -> f b
        fmap f l = L(\p -> 
            let (x, p') = app l p
            in (f x, p'))

    instance Applicative Line where
        -- pure :: a -> f a
        pure x = L(\p -> (x, p))

        -- <*> :: f (a -> b) -> f a -> f b
        fl <*> fx = L (\p -> 
            let (f, p') = app fl p
                (x, p'') = app fx p'
            in
                (f x, p''))

    instance Monad Line where
        -- >>= :: f a -> (a -> f b) -> f b
        xl >>= f = L (\p ->
            let (x, p') = app xl p
            in app (f x) p')

    -- landLeft :: Int -> Line ()
    landLeft n = L (\p -> 
        let (l,r) = p 
        in  if (l + n) - r < 4
            then (Just (), (l + n, r))
            else (Nothing, (l + n, r)))

    -- landLeft :: Int -> Line ()
    landRight n = L (\p -> 
        let (l,r) = p 
        in  if (r + n) - l < 4
            then (Just (), (l, r + n))
            else (Nothing, (l, r + n)))
    
    -- f :: Line ()
    f = do landLeft 2
           landRight 4
           landLeft (-1)
    
    -- g :: Line ()
    g = do landLeft 2
           landRight 4
           landLeft (-1)
           landRight 1
    
    rationale hasFallen lBirds rBirds = 
        if hasFallen /= Nothing 
            then ("Acrobat is on the line with " ++ (show lBirds) ++ " birds on left and " ++ (show rBirds) ++ " birds on the right.")
            else ("Acrobat has fallen: there where " ++ (show lBirds) ++ " birds on left and " ++ (show rBirds) ++ " birds on the right.")

    main = do 
        putStrLn "*** Run 1 ***"
        let (y,p) = (app f (0,0))
            (l,r) = p
        putStrLn (rationale y l r)
        putStrLn "*** Run 2 ***"
        let (y,p) = (app g (0,0))
            (l,r) = p
        putStrLn (rationale y l r)