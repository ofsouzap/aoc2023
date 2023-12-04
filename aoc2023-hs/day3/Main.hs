module Main where

import GHC.Arr (Array, (!), array, listArray, bounds)
import Data.Char (isDigit)
import Utils (splitByNewLineOrdered, readFileLines)

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

-- |Possible neighbours of a position given some bounds
neighbours :: Bounds2d -> (Int, Int) -> [(Int, Int)]
neighbours ((x1, y1), (x2, y2)) (x, y) = filter valid [(x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= dy] where
    valid (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

-- |Find neighbours of a region. Note that this will repeat many values
regionNeighbours :: Bounds2d -> Bounds2d -> [(Int, Int)]
regionNeighbours bounds ((x1, y1), (x2, y2)) = concatMap (neighbours bounds) [(x, y) | x <- [x1..x2], y <- [y1..y2]]

regionIsValid :: Array2d Char -> Bounds2d -> Bool
regionIsValid arr = any (\ pos -> isSymbol (arr ! pos)) . regionNeighbours bs where
    bs = bounds arr

findNumberRegions :: Array2d Char -> [(Int, Bounds2d)]
findNumberRegions arr = aux (bounds arr) start "" [] where
    start = fst (bounds arr)
    aux :: Bounds2d -> (Int, Int) -> String -> [(Int, Bounds2d)] -> [(Int, Bounds2d)]
    aux bounds (x, y) curr acc = [] -- TODO

findInputBounds :: [String] -> Bounds2d
findInputBounds lines = ((x1, y1),  (x2, y2)) where
    (x1, x2) = findXBounds lines
    (y1, y2) = findYBounds lines
    findYBounds xs = (0, length xs - 1)
    findXBounds xs = (0, length (head xs) - 1)

createArray :: [String] -> Array2d Char
createArray lines = array bounds [((x, y), (lines !! y) !! x) | x <- [x1..x2], y <- [y1..y2]] where
    bounds@((x1, y1), (x2, y2)) = findInputBounds lines

-- Solving parts

-- doPartA :: Array2d Char -> Int

-- doPartB :: Array2d Char -> Int

-- Main

mainFor :: String -> String -> IO ()
mainFor name fn = do
    lines <- readFileLines fn
    let arr = createArray lines
    putStrLn "TODO" -- TODO

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
