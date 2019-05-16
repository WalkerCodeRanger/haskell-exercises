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
    "E":severity:timeStamp:msg -> LogMessage (Error (read severity)) (read timeStamp) (unwords msg)
    _ -> Unknown line

-- Parse a whole log file
parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

-- Exercise 2
-- Insert a log message into a sorted log tree. If the message is unkown, return
-- the tree unmodified
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ msgTimeStamp _) (Node left rootMsg@(LogMessage _ rootTimeStamp _) right) =
  if msgTimeStamp < rootTimeStamp then
    Node (insert msg left) rootMsg right
  else
    Node left rootMsg (insert msg right)
insert LogMessage{} (Node _ (Unknown _) _) = error "MessageTree contains Unknown"

-- Exercise 3
-- Build a message tree from a list of log messages
build :: [LogMessage] -> MessageTree
build msgs = foldl (flip insert) Leaf msgs

-- Exercise 4
-- Given a message tree produce an list of log messages ordered by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder tree = inOrder' tree []
    where
      inOrder' Leaf msgs = msgs
      inOrder' (Node left root right) msgs = (inOrder' left (root:(inOrder' right msgs)))

-- Exercise 5

-- Returns errors of severity 50 or higher, sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = ((map message) . inOrder . build . filter (isSevereError 50)) msgs

-- Whether it is an error with at least the minimum severity
isSevereError :: Int -> LogMessage -> Bool
isSevereError minSeverity (LogMessage (Error severity) _ _) = severity >= minSeverity
isSevereError _ _ = False

-- get the message out of a LogMessage
message :: LogMessage -> String
message (LogMessage _ _ msg) = msg
message (Unknown msg) = msg
