module Third where

-- define the function third ...

-- with head and tail
third1 x = head (tail (tail x))

-- with !!
third2 x = x !! 2

-- with pattern matching
third3 (_:_:x:_) = x