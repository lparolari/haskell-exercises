{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

    import Log
    import Control.Applicative
    import Data.Char
    import System.IO

    -- parser with generic type
    newtype Parser a = P (String -> [(a, String)])

    -- parse a string with the parser `p`
    -- renamed in pparse because the collision with parse function used in main.
    pparse :: Parser a -> String -> [(a, String)]
    pparse (P p) inputStr = p inputStr

    -- ******
    -- instancing parser to moands machinery

    -- parser is a functor
    instance Functor Parser where
        -- fmap :: (a -> b) -> f a -> f b
        fmap f p = P (\inputStr -> case pparse p inputStr of
            [] -> []
            [(v, out)] -> [(f v, out)])

    -- parser is an applicative
    instance Applicative Parser where
        -- pure :: a -> f a
        -- <*> :: f (a -> b) -> f a -> f b
        pure v = P (\inputStr -> [(v, inputStr)])
        pg <*> px = P (\inputStr -> case pparse pg inputStr of
            [] -> []
            [(g, out)] -> pparse (fmap g px) out)

    -- parser is a monad
    instance Monad Parser where
        -- return :: a -> f a
        -- >>= :: f a -> (a -> f a) -> f b
        p >>= f = P (\inputStr -> case pparse p inputStr of
            [] -> []
            [(v, out)] -> pparse (f v) out)

    -- parser is an alternative
    instance Alternative Parser where
        -- empty :: Parser a
        empty = P (\inputStr -> [])

        -- (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\inputStr -> case pparse p inputStr of
            [] -> pparse q inputStr
            [(v, out)] -> [(v, out)])

        -- empty examples
        -- pparse empty "abc" --> []
        -- pparse (item <|> return 'd') "abc" --> [('a',"bc")]
        --  because the first succeeds
        -- pparse (empty <|> return 'd') "abc" --> [('d',"abc")]
        --  because empty always fails, so return 'd' is executed

        -- many :: f a -> f [a]
        -- many x = some x <|> pure []

        -- some :: f a -> f [a]
        -- some x = pure (:) <*> x <*> many x

    -- ******
    -- some useful parsers

    -- single character parser
    item :: Parser Char
    item = P (\inputStr -> case inputStr of
        [] -> []
        (x:xs) -> [(x, xs)])

    -- at this time we have
    --   * empty
    --   * return
    --   * item
    -- the three basic parsers.

    -- ******
    -- "library" parsers

    -- single character satifying a predicate parser 
    sat :: (Char -> Bool) -> Parser Char
    sat p = do x <- item
               if p x then return x else empty

    -- two chararters to couple parser
    couple :: Parser (Char, Char)
    couple = do x <- item
                y <- item
                return (x, y)

    -- three characters to triple parser
    triple :: Parser (Char, Char, Char)
    triple = do x <- item
                y <- item
                z <- item
                return (x, y, z)

    -- example (Lesson 21/11/19)
    three = pure g <*> item <*> item <*> item
            where g x y z = (x,z)
    -- g è la g
    -- x prende il valore di g applicato al primo item
    -- y prende il risultato di prima applicato ad item
    -- z prende il risultato di prima applicato ad item
    -- star prende g fuori dal box e la applica a item ...
    --   Attenzione! Il tipo della g:
    -- pure g :: Parser (a -> b -> c -> (a,c))
    -- pure g <*> item :: Parser (b -> c -> (Char, c))
    -- pure g <*> item <*> item :: Parser (b -> (Char, c))
    -- pure g <*> item <*> item <*> item :: Parser (Char, Char)

    -- digit parser
    -- isDigit :: Char -> Bool
    digit :: Parser Char
    digit = sat isDigit

    -- lower case char parser
    -- isLower :: Char -> Bool
    lower :: Parser Char
    lower = sat isLower

    -- upper case char parser
    -- isUpper :: Char -> Bool
    upper :: Parser Char
    upper = sat isUpper

    -- letter char parser
    -- isLetter :: Char -> Bool
    letter :: Parser Char
    letter = sat isLetter

    -- alphanum char parser
    -- isAlphaNum :: Char -> Bool
    alphanum :: Parser Char
    alphanum = sat isAlphaNum
    
    -- specific char parser
    char :: Char -> Parser Char
    char x = sat (==x)

    -- specific string parser
    string :: String -> Parser String
    string [] = return []
    string (x:xs) = do char x
                       string xs
                       return (x:xs)

    -- identifier parser
    identifier :: Parser String
    identifier = do xs <- some alphanum
                    return xs

    nat :: Parser Int
    nat = do xs <- many digit
             return (read xs)
    space :: Parser ()
    space = do many (sat isSpace)
               return ()

    -- ******
    -- special purpose parsers

    -- log level parser (Info, Warning, Error N)
    logLevel :: Parser MessageType
    logLevel = 
        let info = pure g <*> (sat (\c -> c=='I')) where g x = Info
            warn = pure g <*> (sat (\c -> c=='W')) where g x = Warning
            err  = pure g <*> (sat (\c -> c=='E')) <*> (space) <*> (nat) where g x y z = (Error z)
        in info <|> warn <|> err

    -- log timestamp parser: parses an integer (TimeStamp)
    logTimestamp :: Parser TimeStamp
    logTimestamp = nat

    -- log message parser: parses the rest of the string
    logMessage :: Parser String
    logMessage = many item

    -- unknown log parser
    unknownLog :: Parser String
    unknownLog = many item

    -- single log message parser
    message :: Parser LogMessage
    message = do logLevel <- logLevel
                 space
                 logTimestamp <- logTimestamp
                 space
                 logMessage <- logMessage
                 return (LogMessage logLevel logTimestamp logMessage)
              <|> 
              do unknownLog <- unknownLog
                 return (Unknown unknownLog)
    
    app :: Parser a -> String -> a
    app p = \s -> let [(v,"")] = (pparse p) s in v

    stringToLog :: String -> LogMessage
    stringToLog s = app message s

    parse :: String -> [LogMessage]
    parse s = let ls = lines s in
        map (\l -> stringToLog l) ls
    

    -- ***********************************
    -- ordering logs

    insert :: LogMessage -> MessageTree -> MessageTree
    insert (Unknown _) mtree = mtree
    insert log Leaf = Node Leaf log Leaf
    insert log mtree =
        let (LogMessage _ ts _) = log
            (Node lx nodeLog rx) = mtree
            (LogMessage _ nts _) = nodeLog in
        if ts <= nts 
            then Node (insert log lx) nodeLog rx
            else Node lx nodeLog (insert log rx)

    build :: [LogMessage] -> MessageTree
    build ls = foldl (\mt -> \l -> insert l mt) Leaf ls

    inOrder :: MessageTree -> [LogMessage]
    inOrder Leaf = []
    inOrder (Node lx log rx) = (inOrder lx) ++ [log] ++ (inOrder rx)