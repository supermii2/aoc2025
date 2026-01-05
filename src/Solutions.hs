module Solutions (lookupSolution) where

import qualified Solutions.Day1.Part1   as D1a
import qualified Solutions.Day1.Part2   as D1b
import qualified Solutions.Day2.Part1   as D2a
import qualified Solutions.Day2.Part2   as D2b
import qualified Solutions.Day3.Part1   as D3a
import qualified Solutions.Day3.Part2   as D3b
import qualified Solutions.Day4.Part1   as D4a
import qualified Solutions.Day4.Part2   as D4b
import qualified Solutions.Day5.Part1   as D5a
import qualified Solutions.Day5.Part2   as D5b
import qualified Solutions.Day6.Part1   as D6a
import qualified Solutions.Day6.Part2   as D6b
import qualified Solutions.Day7.Part1   as D7a
import qualified Solutions.Day7.Part2   as D7b
import qualified Solutions.Day8.Part1   as D8a
import qualified Solutions.Day8.Part2   as D8b
import qualified Solutions.Day9.Part1   as D9a
import qualified Solutions.Day9.Part2   as D9b
import qualified Solutions.Day10.Part1  as D10a
import qualified Solutions.Day10.Part2  as D10b
import qualified Solutions.Day11.Part1  as D11a
import qualified Solutions.Day11.Part2  as D11b
import qualified Solutions.Day12.Part1  as D12a
import qualified Solutions.Day12.Part2  as D12b

lookupSolution :: Int -> Char -> Maybe (String -> String)
lookupSolution n c = case (n, c) of
    (1, 'a')  -> Just D1a.solve
    (1, 'b')  -> Just D1b.solve
    (2, 'a')  -> Just D2a.solve
    (2, 'b')  -> Just D2b.solve
    (3, 'a')  -> Just D3a.solve
    (3, 'b')  -> Just D3b.solve
    (4, 'a')  -> Just D4a.solve
    (4, 'b')  -> Just D4b.solve
    (5, 'a')  -> Just D5a.solve
    (5, 'b')  -> Just D5b.solve
    (6, 'a')  -> Just D6a.solve
    (6, 'b')  -> Just D6b.solve
    (7, 'a')  -> Just D7a.solve
    (7, 'b')  -> Just D7b.solve
    (8, 'a')  -> Just D8a.solve
    (8, 'b')  -> Just D8b.solve
    (9, 'a')  -> Just D9a.solve
    (9, 'b')  -> Just D9b.solve
    (10, 'a') -> Just D10a.solve
    (10, 'b') -> Just D10b.solve
    (11, 'a') -> Just D11a.solve
    (11, 'b') -> Just D11b.solve
    (12, 'a') -> Just D12a.solve
    (12, 'b') -> Just D12b.solve
    _         -> Nothing