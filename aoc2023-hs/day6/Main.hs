module Main where

import Utils (splitByNewLineOrdered, splitByOrdered)

inputFileName :: String
inputFileName = "inputs/day6.txt"

testInputFileName :: String
testInputFileName = "inputs/day6-test.txt"

type Race = (Int, Int)

-- Reading Inputs

readNs :: String -> [Int]
readNs s = let parts = splitByOrdered (':' ==) s in
    let nsParts = splitByOrdered (' ' ==) (parts !! 1) in
        map read (filter ("" /=) nsParts)

readInput :: String -> [Race]
readInput s = let parts = splitByNewLineOrdered s in
    zip (readNs (head parts)) (readNs (parts !! 1))

readNLong :: String -> Int
readNLong s = let parts = splitByOrdered (':' ==) s in
    read (filter (' ' /=) (parts !! 1))

readInputAsSingle :: String -> Race
readInputAsSingle s = let parts = splitByNewLineOrdered s in
    (readNLong (head parts), readNLong (parts !! 1))

-- Computation

-- solsToBoundedInts :: Race -> (Double, Double) -> (Int, Int)
-- solsToBoundedInts (tr, _) (a, b) = (max 0 solMin, min tr solMax) where
--     solMinD = min a b - 1e-3
--     solMaxD = max a b + 1e+3
--     solMin = ceiling solMinD
--     solMax = floor solMaxD

-- solNeg :: (Double, Double, Double) -> Double
-- solNeg (a, b, c) = (- b - sqrt ((b*b) - (4 * a * c))) / (2 * a)

-- solPos :: (Double, Double, Double) -> Double
-- solPos (a, b, c) = (- b + sqrt ((b*b) - (4 * a * c))) / (2 * a)

-- sols :: (Double, Double, Double) -> (Double, Double)
-- sols quad = (solNeg quad, solPos quad)

quadPos :: (Int, Int, Int) -> Int -> Bool
quadPos q x = quadApp q x > 0 where
    quadApp (a, b, c) x = a*(x*x) + b*x + c

winningCount :: Race -> Int
winningCount r@(tr, dBest) = length (filter (quadPos quad) [0..tr]) where
    quad = (-1, tr, -dBest)

racesWinningCount :: [Race] -> Int
racesWinningCount = foldl (\ acc r -> acc * winningCount r) 1

-- Parts

doPartA :: [Race] -> Int
doPartA = racesWinningCount

doPartB :: Race -> Int
doPartB = winningCount

-- Main

x = "Time:      7  15   30\n\
\Distance:  9  40  200"

input = readInput x

longInput = readInputAsSingle x

mainFor :: String -> FilePath -> IO ()
mainFor name fn = do
    s <- readFile fn
    let input = readInput s
    let partA = doPartA input
    putStrLn ("Part A (" ++ name ++ "): " ++ show partA)
    let longInput = readInputAsSingle s
    let partB = doPartB longInput
    putStrLn ("Part B (" ++ name ++ "): " ++ show partB)

main :: IO ()
main = do
    mainFor "Test" testInputFileName
    mainFor "Main" inputFileName
