import Data.Function ((&))
import Data.List.Split (splitOn, splitWhen)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.List.Split.Internals as T

parse input = lines input

part1 input = "pt1"
part2 input = "pt2"

--     https://adventofcode.com/2025/day/6
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/6/input.txt"

  parse contents & part1 & print
  parse contents & part2 & print
