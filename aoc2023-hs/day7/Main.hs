{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Map (Map, empty, insert, lookup, update, member)
import Data.Maybe (maybe, fromMaybe)
import Utils (splitByNewLineOrdered, splitByOrdered)
import Data.List (sortBy)

inputFileName :: String
inputFileName = "inputs/day7.txt"

testInputFileName :: String
testInputFileName = "inputs/day7-test.txt"

-- Hands and orderings

data Card = Num Int | Joker

instance Show Card where
    show :: Card -> String
    show (Num 14) = "A"
    show (Num 13) = "K"
    show (Num 12) = "Q"
    show (Num 11) = "J"
    show (Num 10) = "T"
    show (Num n) = show n
    show Joker = "Joker"

instance Eq Card where
    (==) :: Card -> Card -> Bool
    Joker == Joker = True
    Num a == Num b = a == b
    _ == _ = False

instance Ord Card where
    (<=) :: Card -> Card -> Bool
    Joker <= Joker = True
    Num a <= Num b = a <= b
    Joker <= Num _ = True
    Num _ <= Joker = False

minCardVal :: Int
minCardVal = 2

maxCardVal :: Int
maxCardVal = 14

readCard :: Char -> Card
readCard 'A' = Num 14
readCard 'K' = Num 13
readCard 'Q' = Num 12
readCard 'J' = Num 11
readCard 'T' = Num 10
readCard n = Num (read [n])

readCardJs :: Char -> Card
readCardJs 'J' = Joker
readCardJs x = readCard x

possibleCards :: [Card]
possibleCards = map Num [minCardVal..maxCardVal]

data HandType = FiveKind | FourKind | FullHouse | ThreeKind | TwoPairs | OnePair | HighCard

handTypeRank :: HandType -> Int
handTypeRank HighCard = 0
handTypeRank OnePair = 1
handTypeRank TwoPairs = 2
handTypeRank ThreeKind = 3
handTypeRank FullHouse = 4
handTypeRank FourKind = 5
handTypeRank FiveKind = 6

instance Eq HandType where
    (==) :: HandType -> HandType -> Bool
    a == b = handTypeRank a == handTypeRank b

instance Ord HandType where
    (<=) :: HandType -> HandType -> Bool
    a <= b = handTypeRank a <= handTypeRank b

data Hand = Hand {
    cards :: [Card],
    handType :: HandType
}

instance Show Hand where
    show = show . cards

type CardCounts = Map Card Int

readCardCounts :: CardCounts -> Card -> Int
readCardCounts m c = fromMaybe 0 (Data.Map.lookup c m)

countCards :: [Card] -> CardCounts
countCards = foldl inc empty where
    inc m c = if member c m
        then update (\ x -> Just (x+1)) c m
        else insert c 1 m

readJokerCount :: CardCounts -> Int
readJokerCount m = readCardCounts m Joker

countsHasCount :: Int -> CardCounts -> Bool
countsHasCount n m = n `elem` map (readCardCounts m) possibleCards

isFiveKind :: CardCounts -> Bool
isFiveKind m = let j = readJokerCount m in
    countsHasCount (5-j) m

isFourKind :: CardCounts -> Bool
isFourKind m = let j = readJokerCount m in
    countsHasCount (4-j) m

isFullHouse :: CardCounts -> Bool
isFullHouse m = let j = readJokerCount m in
    countsHasCount (3-j) m && countsHasCount (2-j) m

isThreeKind :: CardCounts -> Bool
isThreeKind m = let j = readJokerCount m in
    countsHasCount (3-j) m

isTwoPairs :: CardCounts -> Bool
isTwoPairs m = or [canMakeWith n1 n2 | n1 <- possibleCards, n2 <- possibleCards, n1 /= n2] where
    count = readCardCounts m
    j = readJokerCount m
    canMakeWith n1 n2 = let (d1, d2) = (2 - count n1, 2 - count n2) in
        j >= (d1 + d2)

isOnePair :: CardCounts -> Bool
isOnePair m = let j = readJokerCount m in
    countsHasCount (2-j) m

findHandType :: [Card] -> HandType
findHandType cs = aux counts where
    counts = countCards cs
    aux x
        | isFiveKind x = FiveKind
        | isFourKind x = FourKind
        | isFullHouse x = FullHouse
        | isThreeKind x = ThreeKind
        | isTwoPairs x = TwoPairs
        | isOnePair x = OnePair
        | otherwise = HighCard

createHand :: [Card] -> Hand
createHand cs = Hand { cards=cs, handType=findHandType cs }

compCardsEq :: [Card] -> [Card] -> Bool
compCardsEq (ah:ats) (bh:bts) = (ah == bh) && compCardsLeq ats bts
compCardsEq [] [] = True
compCardsEq _ _ = False

compCardsLeq :: [Card] -> [Card] -> Bool
compCardsLeq (ah:ats) (bh:bts) = if ah == bh then compCardsLeq ats bts else ah <= bh
compCardsLeq _ _ = True

instance Eq Hand where
    (==) :: Hand -> Hand -> Bool
    a == b = htA == htB && compCardsEq (cards a) (cards b) where
        htA = handType a
        htB = handType b

instance Ord Hand where
    (<=) :: Hand -> Hand -> Bool
    a <= b = htA < htB || (htA == htB && compCardsLeq (cards a) (cards b)) where
        htA = handType a
        htB = handType b

-- Reading Input

readInputLine :: (Char -> Card) -> String -> (Hand, Int)
readInputLine cardReader s = let parts = splitByOrdered (' ' ==) s in
    ((createHand . map cardReader) (head parts), read (parts !! 1))

readInput :: (Char -> Card) -> String -> [(Hand, Int)]
readInput cardReader = map (readInputLine cardReader) . splitByNewLineOrdered

-- Processing

orderThem :: [(Hand, Int)] -> [(Hand, Int)]
orderThem = sortBy (\ a b -> compare (fst a) (fst b))

scoreOrderedByInvRank :: [(Hand, Int)] -> Int
scoreOrderedByInvRank = fst . foldl foldFunc (0, 1) where
    foldFunc :: (Int, Int) -> (Hand, Int) -> (Int, Int)
    foldFunc (acc, n) (_, bid) = (acc + (n * bid), n+1)

-- Doing parts

doPartA :: [(Hand, Int)] -> Int
doPartA = scoreOrderedByInvRank . orderThem

doPartB :: [(Hand, Int)] -> Int
doPartB = doPartA

-- Main

-- x = "32T3K 765\n\
-- \T55J5 684\n\
-- \KK677 28\n\
-- \KTJJT 220\n\
-- \QQQJA 483"

-- input = readInput readCard x
-- inputJs = readInput readCardJs x

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    s <- readFile fn
    let input = readInput readCard s
    let partA = doPartA input
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)
    let inputJs = readInput readCardJs s
    let partB = doPartB inputJs
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName

