module Solutions.Day4.Part2 (solve) where

solve :: String -> String
solve input =
    let parsedInput = parse input
    in show (neighborGridUntilNoChange parsedInput)

parse :: String -> [[Bool]]
parse input =
    map (map (== '@')) (lines input)

neighbors :: [[Bool]] -> Int -> Int -> Bool
neighbors grid i j =
  (grid !! i !! j) && length
    [ ()
    | di <- [-1..1]
    , dj <- [-1..1]
    , (di, dj) /= (0, 0)
    , let ni = i + di
    , let nj = j + dj
    , ni >= 0, nj >= 0
    , ni < length grid
    , nj < length (head grid)
    , (grid !! ni !! nj)
    ] < 4

neighborGrid :: [[Bool]] -> [[Bool]]
neighborGrid grid =
  [ [ neighbors grid i j
    | j <- [0 .. length (head grid) - 1]
    ]
  | i <- [0 .. length grid - 1]
  ]

removeGrid :: [[Bool]] -> [[Bool]] -> [[Bool]]
removeGrid boxGrid removedGrid =
  zipWith (zipWith (\x y -> x && not y)) boxGrid removedGrid

neighborGridUntilNoChange :: [[Bool]] -> Int
neighborGridUntilNoChange grid =
    let removedGrid = neighborGrid grid
        numRemovable = sum (map (length . filter id) removedGrid)
        nextGrid = removeGrid grid removedGrid
    in if (numRemovable /= 0)
      then numRemovable + (neighborGridUntilNoChange nextGrid)
      else 0