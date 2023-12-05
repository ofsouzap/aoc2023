{-# LANGUAGE TupleSections #-}
module Main where

import GHC.Arr (Array, (!), array, listArray, bounds)
import Data.Char (isDigit)
import Utils (splitByNewLineOrdered, readFileLines)
import Data.List (nub)

-- Setup Stuffs

type Array2d = Array (Int, Int)
type Bounds2d = ((Int, Int), (Int, Int))

inputFileName :: String
inputFileName = "inputs/day3.txt"

testInputFileName :: String
testInputFileName = "inputs/day3-test.txt"

-- Functionality stuffs

isSymbol :: Char -> Bool
isSymbol c = (c /= '.') && not (isDigit c)

isGearSymbol :: Char -> Bool
isGearSymbol c = c == '*'

getRowList :: Int -> Array2d a -> [a]
getRowList i arr = [arr ! (x, i) | x <- [x1..x2]] where
    ((x1, _), (x2, _)) = bounds arr

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

-- |Possible neighbours of a position given some bounds
neighbours :: Bounds2d -> (Int, Int) -> [(Int, Int)]
neighbours ((x1, y1), (x2, y2)) (x, y) = filter valid [(x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0] where
    valid (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- |Find unique neighbours of a region
regionNeighbours :: Bounds2d -> Bounds2d -> [(Int, Int)]
regionNeighbours bounds ((x1, y1), (x2, y2)) = nub (concatMap (neighbours bounds) [(x, y) | x <- [x1..x2], y <- [y1..y2]])

regionIsValid :: Array2d Char -> Bounds2d -> Bool
regionIsValid arr = any (\ pos -> isSymbol (arr ! pos)) . regionNeighbours bs where
    bs = bounds arr

findNumberRegions :: Array2d Char -> [(Int, Bounds2d)]
findNumberRegions arr = concatMap applyLast [(searchLabelledRow . labelRow y . getRowList y) arr | y <- [y1..y2]] where
    ((x1, y1), (x2, y2)) = bounds arr

    labelRow :: Int -> [Char] -> [((Int, Int), Char)]
    labelRow y = zip (map (, y) [0..])

    searchLabelledRow :: [((Int, Int), Char)] -> (Maybe (String, Bounds2d), [(Int, Bounds2d)])
    searchLabelledRow = foldl foldFunc (Nothing, [])

    foldFunc :: (Maybe (String, Bounds2d), [(Int, Bounds2d)]) -> ((Int, Int), Char) -> (Maybe (String, Bounds2d), [(Int, Bounds2d)])
    foldFunc (Nothing, xs) (pos, c) = if isDigit c
        then (Just ([c], (pos, pos)), xs)
        else (Nothing, xs)
    foldFunc (Just (accRev, accBounds@(accStart, _)), xs) (pos, c) = if isDigit c
        then (Just (c : accRev, (accStart, pos)), xs)
        else (Nothing, (read (reverse accRev), accBounds) : xs)

    applyLast :: (Maybe (String, Bounds2d), [(Int, Bounds2d)]) -> [(Int, Bounds2d)]
    applyLast (Nothing, xs) = xs
    applyLast (Just (accRev, accBounds), xs) = (read (reverse accRev), accBounds) : xs

findPartsFromNumberRegions :: Array2d Char -> [(Int, Bounds2d)] -> [Int]
findPartsFromNumberRegions arr regs = map fst (filter filterFunc regs) where
    filterFunc (_, nBounds) = (any (isSymbol . (arr !)) . regionNeighbours arrBounds) nBounds
    arrBounds = bounds arr

scoreGears :: Array2d Char -> Int
scoreGears arr = sum [(scoreLabelledRow . labelRow y . getRowList y) arr | y <- [y1..y2]] where
    arrBounds@((_, y1), (_, y2)) = bounds arr

    numberRegions :: [(Int, Bounds2d)]
    numberRegions = findNumberRegions arr

    labelRow :: Int -> [Char] -> [((Int, Int), Char)]
    labelRow y = zip (map (, y) [0..])

    scoreLabelledRow :: [((Int, Int), Char)] -> Int
    scoreLabelledRow = sum . map scoreForGear

    scoreForGear :: ((Int, Int), Char) -> Int
    scoreForGear (pos, c) = if isGearSymbol c then scoreNumberNeighbours pos else 0

    scoreNumberNeighbours :: (Int, Int) -> Int
    scoreNumberNeighbours pos = let ns = numberNeighbours pos in
        if length ns == 2 then product ns else 0

    numberNeighbours :: (Int, Int) -> [Int]
    numberNeighbours pos = (map fst . filter (regionBesidePos pos . snd)) numberRegions

    regionBesidePos :: (Int, Int) -> Bounds2d -> Bool
    regionBesidePos pos region = pos `elem` regionNeighbours arrBounds region

findInputBounds :: [String] -> Bounds2d
findInputBounds lines = ((x1, y1), (x2, y2)) where
    (x1, x2) = findXBounds lines
    (y1, y2) = findYBounds lines
    findYBounds xs = (0, length xs - 1)
    findXBounds xs = (0, length (head xs) - 1)

createArray :: [String] -> Array2d Char
createArray lines = array bounds [((x, y), (lines !! y) !! x) | x <- [x1..x2], y <- [y1..y2]] where
    bounds@((x1, y1), (x2, y2)) = findInputBounds lines

-- Solving parts

doPartA :: Array2d Char -> Int
doPartA arr = sum (findPartsFromNumberRegions arr (findNumberRegions arr))

doPartB :: Array2d Char -> Int
doPartB = scoreGears

-- Main

s = "467..114..\n\
\...*......\n\
\..35..633.\n\
\......#...\n\
\617*......\n\
\.....+.58.\n\
\..592.....\n\
\......755.\n\
\...$.*....\n\
\.664.598.."

arr = createArray (splitByNewLineOrdered s)

mainFor :: String -> String -> IO ()
mainFor name fn = do
    lines <- readFileLines fn
    let arr = createArray lines
    let partA = doPartA arr
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)
    let partB = doPartB arr
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
