{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1

-- Parse a log line
parseMessage :: String -> LogMessage
parseMessage line = let parts = (words line) in
  case parts of
    "I":timeStamp:msg -> LogMessage Info (read timeStamp) (unwords msg)
    "W":timeStamp:msg -> LogMessage Warning (read timeStamp) (unwords msg)
    "E":errorNum:timeStamp:msg -> LogMessage (Error (read errorNum)) (read timeStamp) (unwords msg)
    _ -> Unknown line

-- Parse a whole log file
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)
