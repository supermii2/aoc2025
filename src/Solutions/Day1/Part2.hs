module Solutions.Day1.Part2 (solve) where

solve :: String -> String
solve input =
    let parsedInput = parse input
        numZeroes = countZeroPasses parsedInput
    in show numZeroes

parse :: String -> [Int]
parse input =
    let turns = lines input
    in map (\a -> (
        case a of 
            (c:rest) -> (
                let n = read rest :: Int 
                in case c of
                    'L' -> -n
                    'R' -> n
                    _ -> error "Invalid Input"
                )
            _ -> error "Invalid Input"
        )) turns

processTurn :: (Int, Int) -> Int -> (Int, Int)
processTurn (currentPosition, numPasses) turn =
    let freeTurns = abs (quot turn 100)
        remTurns = rem turn 100
        nextPos = currentPosition + remTurns
        isRemTurnPass = if ( currentPosition /= 0 && (nextPos <= 0 || nextPos >= 100)) then 1 else 0
        nextNumPasses = numPasses + freeTurns + isRemTurnPass
        nextPosMod = mod nextPos 100
    in (nextPosMod, nextNumPasses)

countZeroPasses :: [Int] -> Int
countZeroPasses [] = 0
countZeroPasses xs =
    let (_, passes) = foldl (\state turn -> processTurn state turn) (50, 0) xs
    in passes