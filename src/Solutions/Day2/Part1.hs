module Solutions.Day2.Part1 (solve) where

import qualified Data.Set as Set

solve :: String -> String
solve input = 
    let ranges = parse input
        invalidIdSet = foldl 
            (\workingSet range -> Set.union workingSet (invalidsPerRange range))
            (Set.empty :: Set.Set Int) 
            ranges
    in show (Set.foldl (+) 0 invalidIdSet)

parse :: String -> [[String]]
parse input =
    map (\range -> wordsWhen (=='-') range) (wordsWhen (==',') input)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

lowerBound :: String -> Int
lowerBound lbs =
    if (mod (length lbs) 2 == 1)
        then (10 ^ (div (length lbs) 2))
        else (
            let (front, back) = splitAt (div (length lbs) 2) lbs
            in if (front < back) then ((read front) + 1) else (read front)
        )

upperBound :: String -> Int
upperBound ubs =
    if (mod (length ubs) 2 == 1)
        then ((10 ^ (div (length ubs) 2)) - 1)
        else (
            let (front, back) = splitAt (div (length ubs) 2) ubs
            in if (front > back) then ((read front) - 1) else (read front)
        )

invalidsPerRange :: [String] -> Set.Set Int
invalidsPerRange range = 
    let lbs = lowerBound (head range)
        ubs = upperBound (head (tail range))
        invalidBlocks = [lbs..ubs]
        invalidIds = map (\block -> read ((show block) ++ (show block))) invalidBlocks
    in Set.fromList invalidIds

