module Solutions.Day1.Part1 (solve) where

solve :: String -> String
solve input =
    let parsedInput = parse input
        turns = cumulative parsedInput
        numZeroes = countZeroes turns
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

countZeroes :: [Int] -> Int
countZeroes xs = length (filter (== 0) xs)

cumulative :: [Int] -> [Int]
cumulative nums = 
    scanl (\acc x -> (acc + x) `mod` 100) 50 nums
