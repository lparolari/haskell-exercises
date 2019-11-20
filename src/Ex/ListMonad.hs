-- Functors

newtype List a = L [a]
    deriving (Show)

extract :: List a -> [a]
extract (L (x:xs)) = x:xs

instance Functor List where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g (L []) = L []
    fmap g (L (x:xs)) =
        let v = g x in
            let vs = fmap g xs in
                L (v:vs)

instance Applicative List where
    -- pure :: a -> f a
    -- <*> :: f (a -> b) -> f a -> f b
    pure x = L [x]
    L gs <*> L xs = L [g x | g <- gs, x <- xs]

-- what??
--instance Monad List where
    -- return :: a -> m a
    -- >>= :: m a -> (a -> m b) -> m b
    --xs >>= f = L [f x | x <- extract xs]
    -- List xs >>= g = List [g x | x <- xs]


ex1 = 
    let xs = L [1,2,3] in
        fmap (+1) xs
ex2 = 
    let xs = L [1,2,3] in
        L [(+1), (*2)] <*> xs

--class ToMadre tm where
--    mammita :: tm a -> tm b

--instance ToMadre List where
--    mammita x = fmap (\_ -> "mammita") (List x)