module Solutions.Day7.Part1 (solve) where

solve :: String -> String
solve input =
    let parsedLines = parse input
    in show (evaluateLines (head parsedLines) (tail  parsedLines))

parse :: String -> [[Bool]]
parse input =
    map (map (/='.')) (lines input)

evaluateLines :: [Bool] -> [[Bool]] -> Int
evaluateLines _ [] = 0
evaluateLines beams splitters =
    let splitterLine = head splitters
        activatedSplitters = zipWith (&&) beams splitterLine
        count = length (filter id activatedSplitters)
        newBeams = zipWith3 (\l x r -> (l || r) && not x) (False:activatedSplitters) activatedSplitters (tail activatedSplitters ++ [False])
        untouchedBeams = zipWith (\b s -> b && not s) beams splitterLine
        nextBeams = zipWith (||) newBeams untouchedBeams
    in count + (evaluateLines nextBeams (tail splitters))

