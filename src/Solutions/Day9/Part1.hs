module Solutions.Day9.Part1 (solve) where

solve :: String -> String
solve input =
    show $ maximum (map distance (allPairs $ parse input))

parse :: String -> [(Int, Int)]
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

distance :: ((Int, Int), (Int, Int))-> Int
distance ((x1, y1), (x2, y2)) =
  let dx = abs (x2 - x1) + 1
      dy = abs (y2 - y1) + 1
  in dx * dy

