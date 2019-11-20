module GenericMonads where

    -- mapM
    -- filterM
    -- join (concat)

    mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
    mapM' _ [] = return []
    mapM' f (x:xs) = do y <- f x
                        ys <- mapM' f xs
                        return (y:ys)
    
    filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM' _ [] = return []
    filterM' f (x:xs) = let ps = f x in 
        do p <- ps
           ys <- filterM' f xs
           return (if p == True then (x:ys) else ys)
    
    join :: Monad m => m (m a) -> m a
    join xs = do x <- xs
                 y <- x
                 return y