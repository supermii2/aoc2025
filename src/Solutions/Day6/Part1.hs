module Solutions.Day6.Part1 (solve) where

import Data.List (transpose)

solve :: String -> String
solve input =
    let zippedNumOp = parse input
    in show $ sum (map evaluateExp zippedNumOp)

parse :: String -> [([Int], Char)]
parse input =
    let xs = lines input
        nums = parseNums (init xs)
        ops = parseOps (last xs)
    in zip nums ops

parseNums :: [String] -> [[Int]]
parseNums numsArray =
    let nums = map (\numLine -> map read (filter (not . null) (wordsWhen (==' ') numLine))) numsArray
    in transpose nums

parseOps :: String -> [Char]
parseOps opsString = filter (/= ' ') opsString

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

evaluateExp :: ([Int], Char) -> Int
evaluateExp (xs, op) =
    if op == '+' then foldl1 (+) xs else foldl1 (*) xs