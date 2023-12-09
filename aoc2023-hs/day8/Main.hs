module Main where

import Data.Map (Map, empty, insert, lookup, keys)
import Data.Set (Set, empty, insert, fromList, elems, member)
import Utils (splitByNewLineOrdered, splitByOrdered, removeWhitespace)

inputFileName :: String
inputFileName = "inputs/day8.txt"

testInputAFileName :: String
testInputAFileName = "inputs/day8-testa.txt"

testInputBFileName :: String
testInputBFileName = "inputs/day8-testb.txt"

testInputCFileName :: String
testInputCFileName = "inputs/day8-testc.txt"

data Move = L | R

instance Show Move where
    show L = "L"
    show R = "R"

type Node = String

initialNode :: Node
initialNode = "AAA"

finalNode :: Node
finalNode = "ZZZ"

type NodeMap = Map Node (Node, Node)

-- Reading input

readMove :: Char -> Move
readMove 'L' = L
readMove 'R' = R
readMove _ = L

readMoves :: String -> [Move]
readMoves = map readMove

readMapEntry :: String -> (Node, (Node, Node))
readMapEntry s = (start, (l, r)) where
    parts = splitByOrdered ('=' ==) (removeWhitespace s)
    start = head parts
    lrParts = splitByOrdered (',' ==) (drop 1 (parts !! 1))
    l = head lrParts
    rWithBracket = lrParts !! 1
    r = take (length rWithBracket - 1) rWithBracket

readMap :: [String] -> NodeMap
readMap = foldl f Data.Map.empty where
    f :: NodeMap -> String -> NodeMap
    f m line = uncurry Data.Map.insert (readMapEntry line) m

readInput :: String -> ([Move], NodeMap)
readInput s = let parts = splitByNewLineOrdered s in
    (readMoves (head parts), readMap (drop 2 parts))

-- Processing

infMoveSeq :: [Move] -> [Move]
infMoveSeq ms = ms ++ infMoveSeq ms -- Might need optimisation? Unsure

takeMove :: Move -> NodeMap -> Node -> Node
takeMove move m n = case (Data.Map.lookup n m, move) of
    (Nothing, _) -> n
    (Just (nl, _), L) -> nl
    (Just (_, nr), R) -> nr

takeMoveOnMany :: Move -> NodeMap -> Set Node -> Set Node
takeMoveOnMany move m = foldl foldFunc Data.Set.empty . elems where
    foldFunc :: Set Node -> Node -> Set Node
    foldFunc s node = Data.Set.insert (takeMove move m node) s

findSearchLengthForNode :: ([Move], NodeMap) -> Node -> Int
findSearchLengthForNode (xs, m) start = aux (start, 0) xsInf where
    xsInf = infMoveSeq xs
    aux :: (Node, Int) -> [Move] -> Int
    aux _ [] = -1
    aux (node, n) (h:ts) = if node == finalNode
        then n
        else aux (takeMove h m node, n+1) ts

isStartNode :: Node -> Bool
isStartNode (_:_:'A':_) = True
isStartNode _ = False

isEndNode :: Node -> Bool
isEndNode (_:_:'Z':_) = True
isEndNode _ = False

findManyStarts :: NodeMap -> [Node]
findManyStarts m = filter isStartNode (keys m)

validEnd :: Set Node -> Bool
validEnd = all isEndNode

-- Not working yet and probably not useful
-- findZTimes :: ([Move], NodeMap) -> Node -> [Int]
-- findZTimes inp@(xs, m) start = aux (start, [], Data.Set.empty, 1) xs where
--     aux :: (Node, [Int], Set Node, Int) -> [Move] -> [Int]
--     aux (_, ends, _, _) [] = ends
--     aux (node, ends, hist, t) (h:ts)
--         | member node hist = ends
--         | otherwise = let ends' = if isEndNode node then t : ends' else ends in
--             aux (takeMove h m node, ends', Data.Set.insert node hist, t+1) ts

findGhostSearchLength :: ([Move], NodeMap) -> Int
findGhostSearchLength (xs, m) = aux (initialNodesSet, 0) xsInf where
    xsInf = infMoveSeq xs
    initialNodesSet = Data.Set.fromList initialNodes
    initialNodes = findManyStarts m
    aux :: (Set Node, Int) -> [Move] -> Int
    aux _ [] = -1
    aux (nodes, n) (h:ts) = if validEnd nodes
        then n
        else aux (takeMoveOnMany h m nodes, n+1) ts

-- Doing parts

doPartA :: ([Move], NodeMap) -> Int
doPartA inp = findSearchLengthForNode inp initialNode

doPartB :: ([Move], NodeMap) -> Int
doPartB = findGhostSearchLength

-- Main

x1 = "RL\n\
\\n\
\AAA = (BBB, CCC)\n\
\BBB = (DDD, EEE)\n\
\CCC = (ZZZ, GGG)\n\
\DDD = (DDD, DDD)\n\
\EEE = (EEE, EEE)\n\
\GGG = (GGG, GGG)\n\
\ZZZ = (ZZZ, ZZZ)"

x2 = "LLR\n\
\\n\
\AAA = (BBB, BBB)\n\
\BBB = (AAA, ZZZ)\n\
\ZZZ = (ZZZ, ZZZ)"

x3 = "LR\n\
\\n\
\11A = (11B, XXX)\n\
\11B = (XXX, 11Z)\n\
\11Z = (11B, XXX)\n\
\22A = (22B, XXX)\n\
\22B = (22C, 22C)\n\
\22C = (22Z, 22Z)\n\
\22Z = (22B, 22B)\n\
\XXX = (XXX, XXX)"

partAFor :: String -> FilePath -> IO ()
partAFor name fn = do
    s <- readFile fn
    let input = readInput s
    let partA = doPartA input
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)

partBFor :: String -> FilePath -> IO ()
partBFor name fn = do
    s <- readFile fn
    let input = readInput s
    let partB = doPartB input
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    partAFor "Test A" testInputAFileName
    partAFor "Test B" testInputBFileName
    partAFor "Main" inputFileName
    partBFor "Test C" testInputCFileName
    partBFor "Main" inputFileName
