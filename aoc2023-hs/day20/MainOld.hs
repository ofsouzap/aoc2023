{-# LANGUAGE ExistentialQuantification #-}
module MainOld where

import Data.Map as Map
  ( Map
  , empty
  , insert
  , keys
  , lookup )
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

-- Queue stuff

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

-- Data layout

newtype ModuleName = ModuleName String
  deriving (Eq, Show, Ord)

newtype Signal = Signal Bool
  deriving (Eq, Show)

data WireSignal = WireSignal ModuleName Signal
  deriving (Eq, Show)

type WireSignalRequest = (WireSignal, [ModuleName])

-- Module typeclass

class Module a where
  moduleName :: a -> ModuleName
  transition :: WireSignal -> a -> (a, Maybe Signal)
  moduleInps :: a -> [ModuleName]
  moduleOuts :: a -> [ModuleName]

fullModuleTransition :: Module a => WireSignal -> a -> (a, Maybe WireSignal)
fullModuleTransition wireSig m = let (m', sig) = transition wireSig m in (m', WireSignal (moduleName m') sig)

-- Module datas

newtype BroadcasterModule = BroadcasterModule { broadcasterOuts :: [ModuleName] }

createBroadcasterModule :: [ModuleName] -> BroadcasterModule
createBroadcasterModule outs = BroadcasterModule { broadcasterOuts=outs }

data FFModule = FFModule
  { ffName :: ModuleName
  , state :: Bool
  , ffInps :: [ModuleName]
  , ffOuts :: [ModuleName] }

createFFModule :: ModuleName -> [ModuleName] -> FFModule
createFFModule name outs = FFModule { ffName=name, state=False, ffInps=[], ffOuts=outs }

flipFF :: FFModule -> FFModule
flipFF ff = ff { state=not (state ff) }

data ConjModule = ConjModule
  { conjName :: ModuleName
  , mem :: Map ModuleName Bool
  , conjInps :: [ModuleName]
  , conjOuts :: [ModuleName] }

createConjModule :: ModuleName -> [ModuleName] -> ConjModule
createConjModule name outs = ConjModule { conjName=name, mem=Map.empty, conjInps=[], conjOuts=outs }

conjOut :: ConjModule -> Bool
conjOut conj = (all (fromMaybe False . (`Map.lookup` mem conj)) . keys . mem) conj

updateConj :: ConjModule -> ModuleName -> Signal -> ConjModule
updateConj conj src (Signal s) = conj { mem=insert src s (mem conj) }

-- Module instances

instance Module BroadcasterModule where
  moduleName _ = ModuleName "broadcaster"
  transition (WireSignal _ sig) x = (x, Just sig)
  moduleInps _ = []
  moduleOuts = broadcasterOuts

instance Module FFModule where
  moduleName = ffName
  transition (WireSignal _ (Signal s)) ff = if s then (ff, Nothing) else (flipFF ff, (Just . Signal . not) s)
  moduleInps = ffInps
  moduleOuts = ffOuts

instance Module ConjModule where
  moduleName = conjName
  transition (WireSignal src sig@(Signal _)) conj = let conj' = updateConj conj src sig in (conj', (Just . Signal . conjOut) conj')
  moduleInps = conjInps
  moduleOuts = conjOuts

-- Network

data ModuleWrapper = forall a. Module a => ModuleWrapper a

newtype Network = Network [ModuleWrapper]

sendWireSignalToModule :: WireSignal -> ModuleName -> Network -> (Network, [WireSignalRequest])
sendWireSignalToModule wireSig mName (Network ms) = (Network . foldr f []) ms where
  f :: ModuleWrapper -> ([ModuleWrapper], [WireSignalRequest]) -> ([ModuleWrapper], [WireSignalRequest])
  f (ModuleWrapper m) (mAcc, wsAcc) = if moduleName m == mName
    then let (m', wsM) = fullModuleTransition wireSig in (m' : mAcc, maybe wsAcc (: wsAcc) wsM)
    else (m' : mAcc, wsAcc)

applyWireSignalRequest :: WireSignalRequest -> Network -> (Network, [WireSignalRequest])
applyWireSignalRequest (wireSig, dsts) net = foldr f net dsts where
  f :: ModuleName -> Network -> Network
  f mName (Network ms) = undefined -- TODO

-- Reading input

readModuleNbs :: String -> [ModuleName]
readModuleNbs = map ModuleName . splitByOrdered (== ',')

readModuleBase :: String -> [ModuleName] -> ModuleWrapper
readModuleBase ('%':s) outs = ModuleWrapper (createFFModule (ModuleName s) outs)
readModuleBase ('&':s) outs = ModuleWrapper (createConjModule (ModuleName s) outs)
readModuleBase "broadcaster" outs = ModuleWrapper (createBroadcasterModule outs)
readModuleBase _ _ = undefined -- Invalid input

-- |Read a module. Assumes no whitespace!
readModule :: String -> ModuleWrapper
readModule = readModuleParts . splitByOrdered (== '>') . filter (/= '-') where
  readModuleParts xs = readModuleBase (head xs) (readModuleNbs (xs !! 1))

readInput :: String -> Network
readInput = Network . map readModule . splitByNewLineOrdered . removeWhitespace

-- Processing

networkStep :: Network -> Signal -> Network
networkStep rawSig = aux (singletonQueue (WireSignal (ModuleName "button") rawSig, [ModuleName "broadcast"])) where
  aux :: Queue WireSignalRequest -> Network -> Network
  aux q net = case dequeue q of
    Nothing -> net
    Just (wireSigReq, q') -> let (newWireSigs, net') = applyWireSigReq wireSigReq net in
      aux (enqueueList newWireSigs q') net'
  applyWireSigReq :: WireSignalRequest -> Network -> ([WireSignalRequest], Network)
  applyWireSigReq = undefined -- TODO

-- Doing parts

-- doPartA :: ???
-- doPartA inp = undefined

-- doPartB :: ???
-- doPartB = undefined

-- Main

main :: IO ()
main = putStrLn "TODO" -- TODO
