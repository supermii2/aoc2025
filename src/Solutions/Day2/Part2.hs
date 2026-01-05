module Solutions.Day2.Part2 (solve) where

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

lowerBound :: String -> Int -> Int
lowerBound lbs n =
    if (mod (length lbs) n /= 0)
        then (10 ^ (div (length lbs) n))
        else (
            let (front, back) = splitAt (div (length lbs) n) lbs
                frontComparison = concat (replicate (n - 1) front)
            in if (frontComparison < back) then ((read front) + 1) else (read front)
        )

upperBound :: String -> Int -> Int
upperBound ubs n =
    if (mod (length ubs) n /= 0)
        then ((10 ^ (div (length ubs) n)) - 1)
        else (
            let (front, back) = splitAt (div (length ubs) n) ubs
                frontComparison = concat (replicate (n - 1) front)
            in if (frontComparison > back) then ((read front) - 1) else (read front)
        )

invalidsPerRange :: [String] -> Set.Set Int
invalidsPerRange range = 
    let rangeLB = head range
        rangeUB = range !! 1
        numBlocks = [2..length rangeUB]
        invalidBlocks = map (\numBlock ->
            let lbs = lowerBound rangeLB numBlock
                ubs = upperBound rangeUB numBlock
                invalidIds = map (\block -> read (concat (replicate numBlock (show block))) :: Int) [lbs..ubs]
            in Set.fromList invalidIds
            ) numBlocks
    in Set.unions invalidBlocks


