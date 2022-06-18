{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
    "I":(time:rest) -> LogMessage Info (read time) (unwords rest)
    "W":(time:rest) -> LogMessage Warning (read time) (unwords rest)
    "E":(level:(time:rest)) -> LogMessage (Error (read level)) (read time) (unwords rest)
    _ -> Unknown message

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

logTime :: LogMessage -> Int
logTime (Unknown _) = -1
logTime (LogMessage _ t _) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg tree = case tree of
    Leaf -> Node Leaf logMsg Leaf
    Node left nodeLog right -> case (logTime logMsg) <= logTime (nodeLog) of
        True -> Node (insert logMsg left) nodeLog right
        False -> Node left nodeLog (insert logMsg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = (inOrder left) ++ (logMsg : (inOrder right))

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity >= 50
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage (filter isSevere (inOrder (build messages)))