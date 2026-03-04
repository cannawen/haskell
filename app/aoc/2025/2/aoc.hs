import Data.List.Split (splitOn)

parse :: [Char] -> [[Int]]
parse input = map (map read . splitOn "-") (splitOn "," input)

part1 :: [[Int]] -> [String]
part1 input = map show (expandRanges input)
  where
    expandRanges = concatMap (\[from, to] -> [from..to])

--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/2/input-mini.txt"
    let numbers = parse contents
    print numbers

    print (part1 numbers)
