import Data.List.Split (splitOn)

parse :: [Char] -> [[Int]]
parse input = map (map read . splitOn "-") (splitOn "," input)

part1 :: [[Int]] -> [String]
-- part1 = map show $ concatMap (\[from, to] -> [from..to])
part1 input = [show n | [start, end] <- input, n <- [start..end]]

--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/2/input-mini.txt"
    let numbers = parse contents
    print numbers

    print (part1 numbers)
