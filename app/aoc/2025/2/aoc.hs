import Data.List.Split (splitOn)
import Data.Function ((&))

parse :: [Char] -> [[Int]]
parse input = map (map read . splitOn "-") (splitOn "," input)

part1 :: [[Int]] -> Int
part1 input = input
  & concatMap (\[from, to] -> [from..to])
  & map show
  & map (\numberString -> splitAt (length numberString `div` 2) numberString)
  & filter (\(firstHalf, lastHalf) -> firstHalf == lastHalf)
  & map (\(halfNum, _) -> halfNum ++ halfNum)
  & map read
  & sum

--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/2/input-mini.txt"
    let numbers = parse contents
    print numbers

    print (part1 numbers)
