module Tree where

    data Tree a = Nil | Node (Tree a) a (Tree a)
    
    dfs :: Tree a -> [a]
    dfs Nil = []
    dfs (Node l x r) = dfs l ++ [x] ++ dfs r

    -- (Node (Node Nil 2 Nil) 1 (Node Nil 3 Nil))
    --        1
    --     2     3