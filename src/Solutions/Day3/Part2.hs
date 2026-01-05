module Solutions.Day3.Part2 (solve) where

import Data.List (elemIndex)

solve :: String -> String
solve input =
    let parsedInput = parse input
        joltages = map (\jolt -> joltage jolt 12) parsedInput
    in show (foldl (\acc jolt -> acc + (read jolt :: Int)) 0 joltages)
    
parse :: String -> [String]
parse input =
    lines input

joltage :: String -> Int -> String
joltage joltageString n =
    case n of 
        1 -> [maximum joltageString]
        _ -> (
            let (prefix, _) = splitAt ((length joltageString) - n + 1) joltageString
                largestDigit = maximum prefix
                idx = maybe 0 id (elemIndex largestDigit prefix)
                remainder = drop (idx + 1) joltageString
            in ([largestDigit] ++ (joltage remainder (n - 1)))
            )