module TautologiChecker where

    -- tautologies = proposition always true

    data Prop = Const Bool
        | Var Char
        | Not Prop
        | And Prop Prop
        | Imply Prop Prop

    type Assoc k v = [(k,v)]
    type Subst = Assoc Char Bool

    p1 :: Prop
    p1 = And (Var 'A') (Not (Var 'A'))

    p2 :: Prop
    p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

    find :: Subst -> Char -> Bool
    find s c = let [(k,v)] = take 1 (filter (\x -> let (k,v) = x in k == c) s) in v

    eval :: Subst -> Prop -> Bool
    eval _ (Const b) = b
    eval s (Var c) = find s c
    eval s (Not p) = not (eval s p)
    eval s (And p q) = eval s q && eval s q 
    eval s (Imply p q) = eval s p <= eval s q

    isTaut :: Prop -> Bool
    isTaut = eval [('A', True), ('B', False)]