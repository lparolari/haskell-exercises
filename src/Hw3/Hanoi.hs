module Hanoi where

-- tower of hanoi

type Peg = String
type Move = (Peg, Peg)

-- standard hanoi game
-- goal: move N disks from peg a to peg b using c as buffer

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- hanoi game with four pegs
-- goal: move N disks from peg a to peg b using c, d as buffers

hanoiplus :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiplus 0 _ _ _ _ = []
hanoiplus 1 a b _ _ = [(a,b)]
hanoiplus n a b c d =
    hanoiplus (n-2) a c d b
    ++ [(a,d)]
    ++ [(a,b)]
    ++ [(d,b)]
    ++ hanoiplus (n-2) c b a d
