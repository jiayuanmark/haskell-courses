{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Char
import Log

safeRead :: String -> Maybe Int
safeRead w
  | all isDigit w = Just (read w)
  | otherwise     = Nothing

parseMessageType :: String -> (Maybe MessageType, String)
parseMessageType s
  | w == "I"  = (Just Info, rest)
  | w == "W"  = (Just Warning, rest)
  | (w == "E" && length ws >= 1) = case (safeRead (head ws)) of
    Nothing -> (Nothing, s)
    Just e  -> (Just (Error e), unwords (tail ws))
  | otherwise = (Nothing, s)
  where (w:ws)      = words s
        rest        = unwords ws

parseTimeStamp :: String -> (Maybe TimeStamp, String)
parseTimeStamp s
  | length w <= 1 = (Nothing, s)
  | otherwise     = case (safeRead (head w)) of
    Nothing -> (Nothing, s)
    Just t  -> (Just t, unwords (tail w))
  where w = words s

parseMessage :: String -> LogMessage
parseMessage s = case (parseMessageType s) of
  (Nothing, _)  -> Unknown s
  (Just ty, r1) -> case (parseTimeStamp r1) of
    (Nothing, _)  -> Unknown s
    (Just ts, r2) -> LogMessage ty ts r2

parse :: String -> [LogMessage]
parse = map parseMessage . lines

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ t _) = t
getTimeStamp (Unknown _)        = error "cannot get timestamp!"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg         Leaf = Node Leaf msg Leaf
insert msg         (Node lhs mid rhs)
  | tnew < tcur = Node (insert msg lhs) mid rhs
  | otherwise   = Node lhs mid (insert msg rhs)
  where tnew = getTimeStamp msg
        tcur = getTimeStamp mid

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lhs msg rhs) = (inOrder lhs) ++ (msg:(inOrder rhs))

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map msg . filter f . inOrder . build
  where f   (LogMessage (Error e) _ _) = e >= 50
        f   _                          = False
        msg (LogMessage _ _ m)         = m
        msg _                          = error "no message!"
