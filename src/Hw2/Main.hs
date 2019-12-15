module Main where

    import System.IO
    import Log
    import LogAnalysis
    import WhatWentWrong

    getLogContent :: String -> IO String
    getLogContent logFileName = do
        handle <- openFile logFileName ReadMode  
        content <- hGetContents handle
        return content

    getErrorLogContent :: IO String
    getErrorLogContent = getLogContent "error.log"
    
    getSampleLogContent :: IO String
    getSampleLogContent = getLogContent "sample.log"

    myTest = do
        content <- getSampleLogContent
        let logs = (parse content)
        let mtree = build logs
        let logsInOrder = inOrder (mtree)

        putStrLn "LOGS"
        printLoop logs
        putStrLn "MESSAGE TREE"
        print mtree
        putChar '\n'
        putStrLn "LOGS IN ORDER"
        printLoop logsInOrder


    main :: IO()
    main = do
        content <- getSampleLogContent
        printLoop (parse content)

    -- main = do 
    --     handle <- openFile "error.log" ReadMode  
    --     contents <- hGetContents handle
    --     let logLines = lines contents
    --     let test = stringToLog (concat (take 1 logLines))
    --     putStrLn $ show test

    printLoop :: Show a => [a] -> IO()
    printLoop [] = putChar '\n'
    printLoop (x:xs) = do 
        print x
        printLoop xs

    -- | @testParse p n f@ tests the log file parser @p@ by running it
    -- on the first @n@ lines of file @f@.

    testParse :: (String -> [LogMessage]) -> Int -> FilePath -> IO [LogMessage]
    testParse parse n file = take n . parse <$> readFile file

    -- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
    -- warning message extractor @w@ by running them on the log file
    -- @f@.testWhatWentWrong :: (String -> [LogMessage]) -> ([LogMessage] -> [String]) -> FilePath -> IO [String]

    testWhatWentWrong parse whatWentWrong file = whatWentWrong . parse <$> readFile file