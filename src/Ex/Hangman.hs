module Hangman where
    hangman :: IO ()
    hangman = do putStr "Think a word: "
                 word <- getLine
                 putStrLn "Try to guess it: "
                 play word

    play :: String -> IO ()
    play word = do putStr "? "
                   guess <- getLine
                   if guess == word then 
                       putStrLn "You got it!"
                   else
                       do putStrLn (match word guess)
                          play word

    match :: String -> String -> String
    match (_:xs) [] = ['-'] ++ match xs []
    match [] _ = []
    match (x:xs) (y:ys) = 
        let c = (if x == y then x else '-') in
            [c] ++ match xs ys
            