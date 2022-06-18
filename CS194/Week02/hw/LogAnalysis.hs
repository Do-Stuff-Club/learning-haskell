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