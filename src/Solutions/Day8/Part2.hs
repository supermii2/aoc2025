module Solutions.Day8.Part2 (solve) where

import qualified Data.Heap as H
import qualified Data.DisjointSet as DS
import qualified Data.Map.Strict as M

solve :: String -> String
solve input =
    let parsed = parse input
        distheap = heapFromCoords parsed
        uf = buildUF (length parsed)
        mergedEdges = evalUntilConnected (length parsed) uf distheap
        lastTwo = take 2 (reverse mergedEdges)
        (idx1, idx2) = lastTwo !! 0
        (x1, _, _) = parsed !! idx1
        (x2, _, _) = parsed !! idx2
    in show (x1 * x2)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parse :: String -> [Coord]
parse input = map parseLine (lines input)
  where
    parseLine line =
      case map read (wordsWhen (==',') line) of
        [x,y,z] -> (x,y,z)
        _       -> error $ "Invalid line: " ++ line


allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (\y -> (x,y)) xs ++ allPairs xs

type Coord = (Int, Int, Int)
distance :: Coord -> Coord -> Int
distance (x1,y1,z1) (x2,y2,z2) =
    let dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
    in dx*dx + dy*dy + dz*dz

type DistHeap = H.MinHeap (Int, (Int, Int))
heapFromCoords :: [Coord] -> DistHeap
heapFromCoords coords =
    let coordsAndIdx = zip coords [0..]
    in H.fromList [ (distance c1 c2, (idx1, idx2)) | ((c1, idx1), (c2, idx2)) <- allPairs coordsAndIdx ]

type UF = DS.DisjointSet Int

buildUF :: Int -> UF
buildUF n = foldr DS.insert DS.empty [0..(n-1)]

evalUntilConnected :: Int -> UF -> DistHeap -> [(Int, Int)]
evalUntilConnected n uf0 heap = go uf0 heap []
  where
    go :: UF -> DistHeap -> [(Int, Int)] -> [(Int, Int)]
    go ufCurr h merged =
      case H.view h of
        Nothing -> merged
        Just ((_, (i,j)), h') ->
          let repI = DS.representative i ufCurr
              repJ = DS.representative j ufCurr
          in if repI /= repJ
                then 
                  let ufNext = DS.union i j ufCurr
                      merged' = (i,j) : merged
                      reps = map (`DS.representative` ufNext) [0..n-1]
                  in if length (unique reps) == 1
                        then reverse merged'
                        else go ufNext h' merged'
                else go ufCurr h' merged 


unique :: Ord a => [a] -> [a]
unique = M.keys . M.fromList . map (\x -> (x, ()))
