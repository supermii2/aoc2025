module Solutions.Day5.Part2 (solve) where

import Data.List (sortOn)

solve :: String -> String
solve input = 
    let (ranges, _) = parse input
        mergedRanges = sortAndMerge ranges
    in show (sum (map (\(start, end) -> end - start + 1) mergedRanges))

parse :: String -> ([(Int, Int)], [Int])
parse input = parseRanges (lines input)

parseRanges :: [String] -> ([(Int, Int)], [Int])
parseRanges [] = error "Invalid Input"
parseRanges ("":rs) = ([], map read rs)
parseRanges (r:rs) =
    let range = parseRange r
        (ranges, ingredients) = parseRanges rs
    in (range : ranges, ingredients)

parseRange :: String -> (Int, Int)
parseRange rangeString = 
    case wordsWhen (=='-') rangeString of
        (start:end:_) -> (read start, read end)
        _ -> error "Invalid Range"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges (r:rs) = reverse $ foldl merge [r] rs
  where
    merge [] range = [range]
    merge acc@((s1,e1):rest) (s2,e2)
      | s2 <= e1 + 1 = (s1, max e1 e2) : rest
      | otherwise    = (s2,e2) : acc

sortAndMerge :: [(Int, Int)] -> [(Int, Int)]
sortAndMerge ranges = mergeRanges (sortOn fst ranges)