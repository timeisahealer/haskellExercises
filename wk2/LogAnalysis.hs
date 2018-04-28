{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char
-- sToInteger :: String -> Integer
-- sToInteger (x: []) = (toInteger.digitToInt) x
-- sToInteger (x)  =  (sToInteger.init) x * 10 + (toInteger.digitToInt.Last) x

parseMessage :: String -> LogMessage
parseMessage str = let list = words str in
  case list of
    ("I":time:msg) -> LogMessage Info (read time) (unwords msg)
    ("W":time:msg) -> LogMessage Warning (read time) (unwords msg)
    ("E":no:time:msg) -> LogMessage (Error (read no)) (read time) (unwords msg)
    _ -> Unknown (str)

parse :: String -> [LogMessage]
parse str
  | (length.lines) str >= 1 = [parseMessage ((head.lines) str)] ++ parse ((unlines.tail.lines) str)
  | otherwise = []

insert :: LogMessage -> MessageTree -> MessageTree
insert log1@LogMessage{} Leaf = Node Leaf log1 Leaf
insert log1@(LogMessage _ ctime _) (Node left log2@(LogMessage _ ptime _) right)
  | ctime < ptime  = Node (insert log1 left) log2 right
  | otherwise = Node right log2 (insert log1 right)
insert _ tree = tree


build :: [LogMessage] -> MessageTree
build (x: []) = insert x Leaf
build (x: xs) = insert x ((build)xs)
build [] = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lg right) = (inOrder left) ++ [lg] ++ (inOrder right)

getMessages :: [LogMessage] -> [String]
getMessages [] = []
getMessages ((LogMessage _ _ c): xs) = [c] ++ (getMessages xs)
getMessages _ = []

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong lms = ((getMessages) (filter severe ((inOrder.build) lms)))
whatWentWrong = getMessages . inOrder . build . filter (severe)

severe :: LogMessage -> Bool
severe (LogMessage (Error severity) _ _)
  | severity > 50 = True
  | otherwise = False
severe _  = False
