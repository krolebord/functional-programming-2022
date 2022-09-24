module Main (main) where

import Data.Map
    ( delete, empty, findWithDefault, insertWith, null, toList, Map )
import Data.Foldable (find)

type Edges = [(Int, Int)]

data Graph = Graph { nodes :: Map Int [Int], inboundCount :: Map Int Int }

edges :: Int -> Graph -> [Int]
edges node = findWithDefault [] node . nodes

graphFromEdges :: Edges -> Graph
graphFromEdges = Prelude.foldr processEdge (Graph empty empty)
  where
    insertNode :: (Int, Int) -> Map Int [Int] -> Map Int [Int]
    insertNode (from, to) = insertWith (++) from [to] . insertWith (++) to []

    countEdge :: (Int, Int) -> Map Int Int -> Map Int Int
    countEdge (from, to) = insertWith (+) from 0 . insertWith (+) to 1

    processEdge :: (Int, Int) -> Graph -> Graph
    processEdge edge graph = Graph (insertNode edge $ nodes graph) (countEdge edge $ inboundCount graph)

removeNode :: Int -> Graph -> Graph
removeNode node graph = Graph (delete node $ nodes graph) (delete node newInboundCount)
  where
    newInboundCount :: Map Int Int
    newInboundCount = decrementInBound (inboundCount graph) (edges node graph)

    decrementInBound :: Map Int Int -> [Int] -> Map Int Int
    decrementInBound inbound = Prelude.foldl (\count n -> insertWith (+) n (-1) count) inbound

isAcyclic :: Graph -> Bool
isAcyclic = acyclic
  where
    isWithZeroInbound :: (Int, Int) -> Bool
    isWithZeroInbound (_, inbound) = inbound == 0

    acyclic :: Graph -> Bool
    acyclic graph
      | Data.Map.null $ nodes graph = True
      | otherwise = case zeroInboundNode of
          Nothing -> False
          Just (node, _) -> acyclic $ removeNode node graph
          where zeroInboundNode = find isWithZeroInbound $ toList $ inboundCount graph


printEdges :: String -> Edges -> IO ()
printEdges title e = do
  putStrLn $ title ++ ":"
  mapM_ print e

printMap :: (Show a, Show b) => String -> Map a b -> IO ()
printMap title m = do
  putStrLn $ title ++ ":"
  mapM_ print (toList m)

printGraph :: String -> Graph -> IO ()
printGraph title graph = do
  putStrLn $ title ++ ":"
  printMap "Nodes" $ nodes graph
  printMap "Inbound count" $ inboundCount graph
  putStrLn ""


input :: Edges
input = [
  (0, 1),
  (0, 2),
  (1, 2),
  (1, 3),
  (2, 3),
  (2, 5),
  (3, 4),
  (7, 6)
  ];

main :: IO ()
main = do
  printEdges "Input" input
  putStrLn ""

  printGraph "Proccessed graph" graph
  putStrLn ""

  putStrLn $ "Is acyclic: " ++ acyclic
  where
    graph = graphFromEdges input
    acyclic = show . isAcyclic $ graph
