{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
-- module LogAnalysis where
module Main (main) where
import Log
import Text.Read (readMaybe)

parseMessageType :: String -> Maybe MessageType
parseMessageType s = case s of
  (words -> x:y:_) -> case x of
    "I" -> Just Info
    "W" -> Just Warning
    "E" -> case readMaybe y :: Maybe Int of
      Just code -> Just $ Error code
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

parseTimeStamp :: String -> MessageType -> Maybe TimeStamp
parseTimeStamp s mType = case mType of
  Info -> case words s of
    _:y:_ -> readMaybe y :: Maybe Int
    _ -> Nothing
  Warning -> case words s of
    _:y:_ -> readMaybe y :: Maybe Int
    _ -> Nothing
  Error _ -> case words s of
    _:_:y:_ -> readMaybe y :: Maybe Int
    _ -> Nothing

validateAndConcatMsg :: [String] -> Maybe String
validateAndConcatMsg [] = Nothing
validateAndConcatMsg x = Just $ unwords x

parseContent :: String -> MessageType -> Maybe String
parseContent s mType = case mType of
  Info -> case words s of
    _:_:xs -> validateAndConcatMsg xs
    _ -> Nothing
  Warning -> case words s of
    _:_:xs -> validateAndConcatMsg xs
    _ -> Nothing
  Error _ -> case words s of
    _:_:_:xs -> validateAndConcatMsg xs
    _ -> Nothing

parseMessage :: String -> LogMessage
parseMessage s = case parseMessageType s of
  Just mType -> case parseTimeStamp s mType of
    Just t -> case parseContent s mType of
      Just c -> LogMessage mType t c
      _ -> Unknown s
    _ -> Unknown s
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

getTimestamp :: LogMessage -> TimeStamp
-- Unknown msgs have no timestamp. This shouldn't happen as we're already checking this inside insert function.
getTimestamp (Unknown _) = 0
getTimestamp (LogMessage _ ts _) = ts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert logMsgParam t = case t of
  Leaf -> Node Leaf logMsgParam Leaf
  Node _ (Unknown _) _ -> t
  Node left logMsgNode right -> case compare (getTimestamp logMsgParam) (getTimestamp logMsgNode) of
    GT -> Node left logMsgNode (insert logMsgParam right)
    _ -> Node (insert logMsgParam left) logMsgNode right

build2 :: [LogMessage] -> MessageTree -> MessageTree
build2 [] t = t
build2 (x:xs) t = build2 xs $ insert x t

build :: [LogMessage] -> MessageTree
build x = build2 x Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

filterMsgs :: [LogMessage] -> [LogMessage]
filterMsgs [] = []
filterMsgs ((LogMessage (Error e) ts msg):xs)
  | e >= 50 = LogMessage (Error e) ts msg : filterMsgs xs
  | otherwise = filterMsgs xs
filterMsgs (_:xs) = filterMsgs xs

getMsgs :: [LogMessage] -> [String]
getMsgs ((LogMessage _ _ msg):xs) = msg : getMsgs xs
getMsgs _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getMsgs . filterMsgs . inOrder . build

main :: IO()
main = do
  contents <- testWhatWentWrong parse whatWentWrong "error.log"
  mapM_ print contents
