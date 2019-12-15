module WhatWentWrong where

    import Log
    import LogAnalysis

    isCriticalError :: LogMessage -> Bool
    isCriticalError (Unknown _) = False
    isCriticalError (LogMessage mtype _ _) = case mtype of
        (Error n) -> if n >= 50 then True else False
        _ -> False

    getCriticalErrors :: [LogMessage] -> [LogMessage]
    getCriticalErrors ls = filter isCriticalError ls 

    whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong ls = [mstr | (LogMessage _ _ mstr) <- getCriticalErrors ((inOrder . build) ls)]
    
