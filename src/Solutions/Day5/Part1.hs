module Solutions.Day5.Part1 (solve) where

solve :: String -> String
solve input = 
    let (ranges, ingredients) = parse input
        isFreshList = map (\ingredient -> inAnyRange ingredient ranges) ingredients
    in show (length (filter id isFreshList))

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

inAnyRange :: Int -> [(Int, Int)] -> Bool
inAnyRange n ranges = any (\(start, end) -> n >= start && n <= end) ranges
