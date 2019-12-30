module HanoiChecker (check, hanoi) where

    -- Luca Parolari <<luca.parolari23@gmail.com>>
    -- Copyright (C) 2019

    import Data.Char

    -- Hanoi Tower
    -- ===========
    
    -- Hanoi tower puzzle with 4 pegs: naive solution.
    hanoi' :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
    hanoi' 0 _ _ _ _ = []
    hanoi' 1 a b _ _ = [(a,b)]
    hanoi' n a b c d =
           hanoi' (n-2) a c d b
        ++ [(a,d)]
        ++ [(a,b)]
        ++ [(d,b)]
        ++ hanoi' (n-2) c b a d
    
    -- Hanoi tower puzzle with 4 pegs: optimal solution.
    triangular :: Int -> Int
    triangular k = floor (fromIntegral (k * (k+1)) / 2)
    
    allTri :: [(Int,Int)]
    allTri = map (\x -> (x,triangular x)) [1..]
    
    pickx :: Int -> [(Int,Int)] -> Int
    pickx n ((a,b):l) = if b < n then pickx n l else a

    hanoi3 :: Int -> Peg -> Peg -> Peg -> [Move]
    hanoi3 0 _ _ _ = []
    hanoi3 n a b c = hanoi3 (n-1) a c b ++ [(a,b)] ++ hanoi3 (n-1) c b a

    hanoi 0 _ _ _ _ =[]
    hanoi 1 a b c d =[(a,b)]
    hanoi 2 a b c d = [(a,c),(a,b),(c,b)]
    hanoi 3 a b c d = [(a,c),(a,d),(a,b),(d,b),(c,b)]
    hanoi n a b c d = (hanoi (n-x) a c b d) ++ (hanoi3 x a b d) ++ hanoi (n-x) c b a d
        where x = pickx n allTri


    -- Types in HanoiChecker
    -- =====================
    
    -- Peg
    -- --- 
    -- Is the "piolo", i.e. a letter representing the peg.
    
    -- Move
    -- ----
    -- is a pair (Peg, Peg) and represents the "from peg" and "to peg"
    -- for a disk.
    
    -- Config
    -- ------
    -- is a list of lists. To be precise, Config is a list of four 
    -- lists, where every inner list represents the configuration for a peg.
    -- Note that pegs names are mapped on integer with the obvious mapping:
    --  a -> 1, b -> 2, c -> 3, d -> 4, so using the operator !! to access 
    --  the i-th element of a list, we can get one of the four pegs.
    -- Also note that the list representing a peg configuration is a list of
    --  integer from 1 to N, where N is the number of disks.
    -- The integer 1 is the smaller disk in the peg, while the disk N is
    --  the bigger. The ordering between numbers is 1 < 2 < ... < N, i.e
    --  bigger disk cannot stand upon smaller disks.
    
    -- State
    -- -----
    -- is a tuple with the number of moves in the first component and 
    --  Config in the second. 

    type Peg = String
    type Move = (Peg, Peg)
    type Config = [[Int]]
    type State = (Int, Config)  -- (number of moves, config)
    type Info = (Report, [Move])
    data Report = Bad | Ok deriving (Show, Eq)

    newtype ST a = S (State -> (a, State))

    -- instancing ST to Functor, Applicative and Monads

    instance Functor ST where
        -- fmap :: (a -> b) -> f a -> f b
        fmap f st = S (\s -> let (x,s') = (app st) s in (f x, s'))

    instance Applicative ST where
        -- pure :: a -> f a
        pure v = S ( \s -> (v, s) )

        -- <*> :: f (a -> b) -> f a -> f b
        stf <*> sta = S ( \s -> 
            let (f, s') = app stf s
                (v, s'') = app sta s'
            in (f v, s''))

    instance Monad ST where
        -- >>= :: m a -> (a -> m b) -> m b
        sta >>= g = 
            S(\s -> 
                let (v, s') = app sta s
                    (v', s'') = app (g v) s'
                in (v', s''))

    instance Show a => Show (ST a) where
        show (S s) = "S"

    -- Apply a state to a state transformer.
    app :: ST a -> State -> (a, State)
    app (S f) = \s -> f s

    -- Check a list of moves with a given configuration.
    check moves config = app (checkMoves moves) config

    -- Check a list of moves returning an `ST Info`.
    checkMoves :: [Move] -> ST Info
    checkMoves [] = return (Ok, [])
    checkMoves (m:ms) = do
        r <- move m
        if r == Bad 
            then return (Bad, [m])
            else checkMoves ms

    -- Check whether a move is ok and perform the move iff it is correct. 
    move :: Move -> ST Report
    move m = do
        ok <- moveOk m
        if ok 
            then 
                do
                    perform m
                    return Ok
            else 
                return Bad
        
    -- Check whether a move is correct or not.
    moveOk :: Move -> ST Bool
    moveOk move =
        -- ST (State -> (a, State))
        S ( \s -> 
            (
                   (isUsedPegsValid move) 
                && (isMoveNotFromEmptyPeg move s) 
                && (isMoveNotOnBiggerDisk move s), 
            s)
        )
    
    -- Return True whether a disk in moved from an existing peg, False 
    -- otherwise.
    isUsedPegsValid :: Move -> Bool
    isUsedPegsValid move = 
        let allowedPegs = ['a', 'b', 'c', 'd']
            (from, to) = move
        in
            (or (map (\p -> elem p from) allowedPegs)) 
                && (or (map (\p -> elem p to) allowedPegs))
    
    -- Return True whether a disk is not moved from an empty peg, False
    -- otherwise. (i.e., the move is trying to move a non existing disk).
    isMoveNotFromEmptyPeg :: Move -> State -> Bool
    isMoveNotFromEmptyPeg move st = 
        let (from, _) = move
            (_, pegs) = st
        in 
            pegs !! (pegToInt from) /= []

    -- Return True whether a disk is not moved upon a bigger one, False 
    -- otherwise. (i.e., the move is moving a disk on a bigger one).
    isMoveNotOnBiggerDisk :: Move -> State -> Bool
    isMoveNotOnBiggerDisk move st = 
        let (from, to) = move
            (_, pegs) = st
            peg1 = pegs !! (pegToInt from)
            peg2 = pegs !! (pegToInt to)
        in
            -- the check for empty list is not done here, 
            -- should be performed by isMoveNotFromEmptyPeg
            if peg2 == [] 
                then True
                else
                    let (f:_) = peg1
                        (t:_) = peg2
                    in 
                        -- smaller pegs corresponds to 
                        -- smaller numbers
                        f < t
    
    -- Perform the given move.
    perform :: Move -> ST ()
    perform move = 
        -- ST (State -> (a, State))
        -- type Config = [[Int]]
        -- type State = (Int, Config)
        S ( \s ->
            let (i, pegs) = s
                (from, to) = move
                peg1 = pegs !! (pegToInt from)
                peg2 = pegs !! (pegToInt to)
                (f:fs) = peg1
            in
                -- If you are reading this code, I'm sorry with you. But hey, at least it is indented :)
                     if from == "a" && to == "b" then let [_, _, p3, p4] = pegs in ((), (i+1, [fs,     f:peg2, p3,     p4    ]))
                else if from == "a" && to == "c" then let [_, p3, _, p4] = pegs in ((), (i+1, [fs,     p3,     f:peg2, p4    ]))
                else if from == "a" && to == "d" then let [_, p3, p4, _] = pegs in ((), (i+1, [fs,     p3,     p4,     f:peg2]))
                else if from == "b" && to == "a" then let [_, _, p3, p4] = pegs in ((), (i+1, [f:peg2, fs,     p3,     p4    ]))
                else if from == "b" && to == "c" then let [p3, _, _, p4] = pegs in ((), (i+1, [p3,     fs,     f:peg2, p4    ]))
                else if from == "b" && to == "d" then let [p3, _, p4, _] = pegs in ((), (i+1, [p3,     fs,     p4,     f:peg2]))
                else if from == "c" && to == "a" then let [_, p3, _, p4] = pegs in ((), (i+1, [f:peg2, p3,     fs,     p4    ]))
                else if from == "c" && to == "b" then let [p3, _, _, p4] = pegs in ((), (i+1, [p3,     f:peg2, fs,     p4    ]))
                else if from == "c" && to == "d" then let [p3, p4, _, _] = pegs in ((), (i+1, [p3,     p4,     fs,     f:peg2]))
                else if from == "d" && to == "a" then let [_, p3, p4, _] = pegs in ((), (i+1, [f:peg2, p3,     p4,     fs    ]))
                else if from == "d" && to == "b" then let [p3, _, p4, _] = pegs in ((), (i+1, [p3,     f:peg2, p4,     fs    ]))
                else if from == "d" && to == "c" then let [p3, p4, _, _] = pegs in ((), (i+1, [p3,     p4,     f:peg2, fs    ]))
                else ((), s)  -- if the move is correct this case should be useless.
        )

    -- Return a number from 1 to 4 representing given peg string.
    pegToInt :: Peg -> Int
    pegToInt p = ord (p !! 0) - ord 'a'


    -- **************************************************************
    -- TESTS
    -- **************************************************************

    initConfig :: State
    initConfig = (0, [[1,2,3,4],[],[],[]])

    initConfigZ z = (0, [[1..z], [], [], []])

    testMoveOk = do 
        let (ok, s) = (app (moveOk ("a", "b")) initConfig)
        print ok
        print s
        
    testIsUsedPegsValid = do
        print (show (isUsedPegsValid ("a", "b")) ++ " should be true")
        print (show (isUsedPegsValid ("a", "d")) ++ " should be true")
        print (show (isUsedPegsValid ("e", "b")) ++ " should be false")
        print (show (isUsedPegsValid ("a", "e")) ++ " should be false")
        print (show (isUsedPegsValid ("f", "g")) ++ " should be false")

    testIsMoveNotFromEmptyPeg = do
        print (show (isMoveNotFromEmptyPeg ("a", "b") (0, [[1,2,3,4], [], [], []])) ++ " should be true")
        print (show (isMoveNotFromEmptyPeg ("a", "b") (0, [[1], [2,3,4], [], []])) ++ " should be true")
        print (show (isMoveNotFromEmptyPeg ("a", "b") (0, [[], [1,2,3,4], [], []])) ++ " should be false")
        print (show (isMoveNotFromEmptyPeg ("d", "c") (0, [[], [1,2,3,4], [], []])) ++ " should be false")
        
    testIsMoveNotOnBiggerDisk = do
        print (show (isMoveNotOnBiggerDisk ("a", "b") (0, [[1], [2,3,4], [], []])) ++ " should be true")
        print (show (isMoveNotOnBiggerDisk ("a", "b") (0, [[2], [1,3,4], [], []])) ++ " should be false")
        print (show (isMoveNotOnBiggerDisk ("a", "c") (0, [[1], [2,3,4], [], []])) ++ " should be true")
        print (show (isMoveNotOnBiggerDisk ("a", "b") (0, [[1,2], [3,4], [], []])) ++ " should be true")
        print (show (isMoveNotOnBiggerDisk ("a", "b") (0, [[2,3], [1,4], [], []])) ++ " should be false")

    testPerform = do
        let (_, s1) = (app (perform ("a", "b")) initConfig)
            (_, s2) = (app (perform ("a", "c")) initConfig)
        print s1
        print s2

    testMove = do
        let (r1, s1) = (app (move ("a", "b")) initConfig)
            (r2, s2) = (app (move ("a", "c")) initConfig)
        print r1
        print r2

    testCheck = do
        let 
            k1 = [("a","b"),("a","c"),("a","d"),("c","d"),("b","d")]
            -- k1 -> b=1, c=2, d=3, d=2,3, d=1,2,3
            k2 = [("a","b"),("a","c"),("a","d"),("b","d"),("c","d")]
            k3 = [("a","b"),("a","c"),("d","b"),("b","d"),("c","d")]
            (r1, m1) = check k1 (initConfigZ 4)
            (r2, m2) = (app (checkMoves k2) initConfig)
            (r3, m3) = (app (checkMoves k3) initConfig)
        putStrLn (show r1 ++ ": " ++ show m1)
        putStrLn (show r2 ++ ": " ++ show m2)
        putStrLn (show r3 ++ ": " ++ show m3)
    testCheck1 = do
        let k = hanoi 15 "a" "b" "c" "d"
            info = check k (initConfigZ 15)
        putStrLn (show info)
