module Solutions.Day3.Part1 (solve) where

import Data.List (elemIndex)

solve :: String -> String
solve input =
    let parsedInput = parse input
        joltages = map joltage parsedInput
    in show (foldl (+) 0 joltages)
    
parse :: String -> [String]
parse input =
    lines input

joltage :: String -> Int
joltage joltageString =
    let (prefix, _) = splitAt ((length joltageString) - 1) joltageString
        largestDigit = maximum prefix
        idx = maybe 0 id (elemIndex largestDigit prefix)
        remainder = drop (idx + 1) joltageString
        largestDigitNext = maximum remainder
    in read [largestDigit, largestDigitNext] :: Int