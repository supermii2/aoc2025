module Main where

import System.Environment (getArgs)
import Solutions (lookupSolution)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [numStr, [part]] -> do
            let n = read numStr
            case lookupSolution n part of
                Nothing -> putStrLn "Invalid problem/part."
                Just solver -> do
                    input <- readFile ("inputs/" ++ numStr ++ ".txt")
                    putStrLn (solver input)
        _ -> putStrLn "Usage: solver <number> <letter>"
