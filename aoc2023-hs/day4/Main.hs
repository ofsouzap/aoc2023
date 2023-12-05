{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, intersection, elems)
import Data.Map (Map, empty, insert, lookup)
import Data.List (intercalate)
import Utils (splitByNewLineOrdered, splitByOrdered, removeWhitespace, readFileLines)
import HsRegex (Pattern(..), removeStartingMatchLongest)

inputFileName :: String
inputFileName = "inputs/day4.txt"

testInputFileName :: String
testInputFileName = "inputs/day4-test.txt"

data Card = Card {
    cid :: Int, -- Card number
    wins :: Set Int, -- Winning numbers
    haves :: Set Int -- Numbers you have
}

instance Show Card where
    show :: Card -> String
    show c = "Card " ++ show (cid c) -- ++ ": " ++ intercalate ", " ((map show . elems . wins) c) ++ " | " ++ intercalate ", " ((map show . elems . haves) c)

dumCard = Card { cid= -1, wins=fromList [], haves=fromList [] }

-- Reading input

extractCid :: String -> Int
extractCid = read . fromMaybe "" . removeStartingMatchLongest (String "Card") . removeWhitespace

extractNums :: String -> Set Int
extractNums = fromList . map read . filter ("" /=) . splitByOrdered (' ' ==)

lineToCard :: String -> Card
lineToCard line = let parts = splitByOrdered (':' ==) line in
    let numParts = splitByOrdered ('|' ==) (parts !! 1) in
        Card
        { cid=extractCid (head parts),
        wins=extractNums (head numParts),
        haves=extractNums (numParts !! 1) }

readInput :: String -> [Card]
readInput = map lineToCard . splitByNewLineOrdered

-- Processing

cardMatches :: Card -> Int
cardMatches c = length (intersection (wins c) (haves c))

scoreCard :: Card -> Int
scoreCard c = let k = cardMatches c in
    if k == 0 then 0 else 2 ^ (k - 1)

buildCardDict :: [Card] -> Map Int Card
buildCardDict = aux empty where
    aux m [] = m
    aux m (h:ts) = aux (insert (cid h) h m) ts

getCards :: Map Int Card -> [Int] -> [Card]
getCards m = map (fromMaybe dumCard . \ x -> Data.Map.lookup x m)

findNextCids :: Card -> [Int]
findNextCids c = let x = cardMatches c in
    if x > 0 then [cid c+1..cid c+x] else []

getNextCards :: Map Int Card -> Card -> [Card]
getNextCards m = getCards m . findNextCids

-- Doing parts

doPartA :: [Card] -> Int
doPartA = sum . map scoreCard

doPartB :: [Card] -> Int
doPartB cs = aux 0 cs where
        cDict = buildCardDict cs
        aux :: Int -> [Card] -> Int
        aux cards [] = cards
        aux cards (h:ts) = let nexts = getNextCards cDict h in
            aux (cards + 1) (nexts ++ ts)

-- Main

x = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
\Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
\Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
\Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
\Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
\Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

cards = readInput x

cDict = buildCardDict cards

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    lines <- readFileLines fn
    let cards = map lineToCard lines
    let partA = doPartA cards
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)
    let partB = doPartB cards
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
