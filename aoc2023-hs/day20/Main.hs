module Main where

import Data.Map as Map
  ( Map
  , empty
  , insert
  , lookup
  , keys )
import Data.Sequence as Seq
  ( Seq(Empty, (:<|))
  , empty
  , singleton
  , fromList
  , (<|)
  , (><) )
import Utils (removeWhitespace, splitByNewLineOrdered, splitByOrdered)
import Data.Maybe (fromMaybe)

inputFileName :: String
inputFileName = "inputs/day8.txt"

testInputAFileName :: String
testInputAFileName = "inputs/day8-testa.txt"

testInputBFileName :: String
testInputBFileName = "inputs/day8-testb.txt"

-- Queue Stuff

newtype Queue a = Queue (Seq a)

emptyQueue :: Queue a
emptyQueue = Queue Seq.empty

singletonQueue :: a -> Queue a
singletonQueue = Queue . Seq.singleton

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue q) = Queue (x <| q)

enqueueList :: [a] -> Queue a -> Queue a
enqueueList xs (Queue q) = Queue (q >< Seq.fromList xs)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue Empty) = Nothing
dequeue (Queue (h:<|ts)) = Just (h, Queue ts)

-- Data stuffs

type ModuleName = String

data FFModule = FFModule
    { ffName :: ModuleName
    , ffState :: Bool
    , ffOuts :: [ModuleName] }

createFFModule :: ModuleName -> [ModuleName] -> FFModule
createFFModule n outs = FFModule { ffName=n, ffState=False, ffOuts=outs }

data ConjModule = ConjModule
    { conjName :: ModuleName
    , conjMem :: Map ModuleName Bool
    , conjOuts :: [ModuleName] }

createConjModule :: ModuleName -> [ModuleName] -> ConjModule
createConjModule n outs = ConjModule { conjName=n, conjMem=Map.empty, conjOuts=outs }

conjOutSignal :: ConjModule -> Bool
conjOutSignal m = (not . all (\ k -> fromMaybe undefined (Map.lookup k (conjMem m))) . keys . conjMem) m

conjRecvSignalFrom :: ModuleName -> Bool -> ConjModule -> ConjModule
conjRecvSignalFrom src sig m = ( m { conjMem=insert src sig (conjMem m) } )

data Network = Network
  { netBroadcastOuts :: [ModuleName]
  , netFfs :: [FFModule]
  , netConjs :: [ConjModule] }

-- Processing



-- Doing parts

-- doPartA :: ???
-- doPartA inp = undefined

-- doPartB :: ???
-- doPartB = undefined

-- Main

main :: IO ()
main = putStrLn "TODO" -- TODO
