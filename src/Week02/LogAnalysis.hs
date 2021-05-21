{-# OPTIONS_GHC -Wall #-}

module Week02.LogAnalysis
    (
        parseMessage,
        parse,
        insert,
        build,
        inOrder,
        whatWentWrong
    ) where

import Week02.Log

parseMessage :: String -> LogMessage
parseMessage msg = case (words msg) of 
    ("I":timeStamp:rest) -> (LogMessage Info (read timeStamp) (unwords rest))
    ("W":timeStamp:rest) -> (LogMessage Warning (read timeStamp) (unwords rest))
    ("E":code:timeStamp:rest) -> (LogMessage (Error (read code)) (read timeStamp) (unwords rest))
    rest -> (Unknown (unwords rest))

parse :: String -> [LogMessage]
parse filetext = map parseMessage (lines filetext)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree = case msg of
    (Unknown _) -> tree
    (LogMessage _ time _) -> case tree of
        (Node left nodeMsg@(LogMessage _ nodeTime _) right)
            | time < nodeTime -> (Node (insert msg left) nodeMsg right)
            | otherwise -> (Node left nodeMsg (insert msg right))
        _ -> (Node Leaf msg Leaf)


build :: [LogMessage] -> MessageTree
build [] = Leaf
build [a] = Node Leaf a Leaf
build (a:as) = insert a (build as)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

isRelevant :: LogMessage -> Bool
isRelevant msg = case msg of
    (LogMessage (Error level) _ _)
        | level >= 50 -> True
        | otherwise -> False
    _ -> False


getMessageFromLog :: [LogMessage] -> [String]
getMessageFromLog [] = []
getMessageFromLog [msg] = case msg of
    (LogMessage _ _ rest) -> [rest]
    _ -> []
getMessageFromLog (a:as) = (getMessageFromLog [a]) ++ (getMessageFromLog as)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msg = getMessageFromLog( filter isRelevant (inOrder (build msg)) )
