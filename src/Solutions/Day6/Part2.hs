module Solutions.Day6.Part2 (solve) where

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
    let numStrings = splitOnEmpty $ transpose numsArray
    in map (map (read . filter (/= ' '))) numStrings

parseOps :: String -> [Char]
parseOps opsString = filter (/= ' ') opsString

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = foldr (\x acc -> if all (==' ') x then [] : acc else (x : head acc) : tail acc) [[]]

evaluateExp :: ([Int], Char) -> Int
evaluateExp (xs, op) =
    if op == '+' then foldl1 (+) xs else foldl1 (*) xs