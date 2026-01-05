module Solutions.Day7.Part2 (solve) where

solve :: String -> String
solve input =
    let (initial, splitters) = parse input
    in show (evaluateLines initial splitters)

parse :: String -> ([Int], [[Bool]])
parse input =
    let xs = map (map (/='.')) (lines input)
        initial = map fromEnum (head xs)
        splitters = tail xs
    in (initial, splitters)

evaluateLines :: [Int] -> [[Bool]] -> Int
evaluateLines xs [] = sum xs
evaluateLines beams splitters =
    let splitterLine = head splitters
        activatedSplitters = zipWith (\beam splitter -> if splitter then beam else 0) beams splitterLine
        newBeams = zipWith3 (\l x r -> (l + r) * (1 - x))
                    (0 : activatedSplitters)
                    activatedSplitters
                    (tail activatedSplitters ++ [0])
        untouchedBeams = zipWith (\b s -> if s then 0 else b) beams splitterLine
        nextBeams = zipWith (+) newBeams untouchedBeams
    in evaluateLines nextBeams (tail splitters)
