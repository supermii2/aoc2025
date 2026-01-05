module Solutions.Day9.Part2 (solve) where

solve :: String -> String
solve input =
    let parsed = parse input
        pairs = allPairs parsed
        removed = map (\pair -> (pair, rectangleInPolygon parsed pair)) pairs
    in show removed

type Coord = (Int, Int)

parse :: String -> [Coord]
parse input = map parseLine (lines input)
  where
    parseLine line =
      case map read (wordsWhen (==',') line) of
        [x,y] -> (x,y)
        _       -> error $ "Invalid line: " ++ line

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (\y -> (x,y)) xs ++ allPairs xs

distance :: (Coord, Coord)-> Int
distance ((x1, y1), (x2, y2)) =
  let dx = abs (x2 - x1) + 1
      dy = abs (y2 - y1) + 1
  in dx * dy

circularPairs :: [a] -> [(a, a)]
circularPairs [] = []
circularPairs xs = zip xs (tail xs ++ [head xs])


