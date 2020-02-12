module Acrobat2 where

    type Move = (Char, Int)
    type Pole = (Int, Int)
    type State = ([Move], Pole)
    newtype Akr a = K (State -> (a, State))

    moves [] (l,r) = if (abs (l-r)) < 4 then True else False
    moves (x:xs) (l,r) = if (abs (l-r)) >= 4 then False else 
        let (d,n) = x
        in case d of
            'L' -> moves xs (l+n,r)
            'R' -> moves xs (l, r+n)
    
    acrobat = K (\s -> 
        let (ms, p) = s
        in (moves ms p, s))

    app (K f) s = f s 

    main = do
        let (ok, _) = app acrobat ([('L',1),('L',1),('L',1),('R',1),('R',36)],(0,0))
        putStrLn (show ok)
