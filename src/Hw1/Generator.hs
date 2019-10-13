-- Show that [(x,y) | x <- [1,2], y <-[3,4]] can be re-expressed with 2
-- comprehensions with single generators

gen = [(x,y) | x <- [1,2], y <-[3,4]]

lst = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

ok = gen == lst