module BinaryStringTransmitter where

    import Data.Char

    type Bit = Int

    -- foldr g v [] = v
    -- foldr g v (x:xs) = g x (foldr g v xs)

    -- Convert a list of bits in its integer representation.
    bin2int :: [Bit] -> Int
    bin2int xs = foldr (\x y -> x + 2*y) 0 xs

    -- what the foldr do without reverse:
    -- [1,1,0,1] --> 1+2+8 = 11 
    -- [0,1] --> 1

    --   foldr (\x y -> x + 2*y) 0 [0,1]
    -- = g 0 (foldr g 0 [1])
    -- = g 0 (g 1 foldr g 0 [])
    -- = g 0 (g 1 0)
    -- = g 0 (1+2*0)
    -- = g 0 1
    -- = (0+2*1)
    -- = 2
    -- ok, because the encoding is reversed.

    -- Convert an integer in its binary representation.
    int2bin :: Int -> [Bit]
    int2bin 0 = []
    int2bin n = (n `mod` 2) : (int2bin (n `div` 2))
    
    -- Integer idenity.
    intid :: Int -> Int
    intid n = (bin2int . int2bin) n

    -- Binary identity.
    binid :: [Bit] -> [Bit]
    binid xs = (int2bin . bin2int) xs

    -- Trim the bits list to 8 bits.
    make8 :: [Bit] -> [Bit]
    make8 xs = take 8 (xs ++ (repeat 0))

    chop8 :: [Bit] -> [[Bit]]
    chop8 [] = []
    chop8 bits = [take 8 bits] ++ chop8 (drop 8 bits)

    -- Encode a string to bits
    encode :: String -> [Bit]
    encode = concat . map (make8 . int2bin . ord)

    -- Decode a string from bits
    decode :: [Bit] -> String
    decode bits = map (chr . bin2int) (chop8 bits)

    bits2string :: [Bit] -> String
    bits2string = map intToDigit
    
