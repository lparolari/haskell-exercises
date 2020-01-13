module Ex3 where

    data LogicNat = Zero | Succ LogicNat deriving Show

    add Zero n = n
    add (Succ m) n = Succ (add m n)

    toLogic 0 = Zero
    toLogic n = Succ (toLogic (n-1))

    toNormal Zero = 0
    toNormal (Succ n) = toNormal n + 1