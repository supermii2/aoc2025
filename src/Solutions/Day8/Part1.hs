module Solutions.Day8.Part1 (solve) where

import qualified Data.Heap as H
import qualified Data.DisjointSet as DS
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Ord (Down(..))

solve :: String -> String
solve input =
    let parsed = parse input
        distheap = heapFromCoords parsed
        uf = buildUF (length parsed)
        finalUf = evalPops uf distheap 1000
        tls = threeLargestSizes (length parsed) finalUf
    in show (foldl1 (*) tls)

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

evalPops :: UF -> DistHeap -> Int -> UF
evalPops uf _ 0 = uf
evalPops uf heap count =
    let ((_, (idx1, idx2)), newHeap) = maybe (error "Empty Heap") id (H.view heap)
        newUf = DS.union idx1 idx2 uf
    in evalPops newUf newHeap (count - 1)

threeLargestSizes :: Int -> UF -> [Int]
threeLargestSizes n uf =
    let elements = [0..(n-1)]
        reps = map (`DS.representative` uf) elements
        countsMap = foldr (\r m -> M.insertWith (+) r 1 m) M.empty reps
    in take 3 $ sortOn Down (M.elems countsMap)