-- zip
zip' x y = [(a,b) | a <- x, b <- y]

-- map
map' f x = [f a | a <- x]