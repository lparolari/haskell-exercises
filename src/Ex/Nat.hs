module Nat where
    
    data Nat = Zero | S Nat
        deriving (Show)

    int2nat :: Int -> Nat
    int2nat 0 = Zero
    int2nat n = S ( int2nat (n-1) )

    nat2int :: Nat -> Int
    nat2int Zero = 0
    nat2int (S(x)) = 1 + nat2int x
