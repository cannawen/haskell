import Data.Function ((&))
import Data.List.Split (splitOn)
import Data.List (transpose)

parse input = (lines input)
  & map (splitOn " ")
  & map (filter (/= ""))
  & transpose
  & map reverse

part1 input = "pt1"

part2 input = "pt2"

--     https://adventofcode.com/2025/day/6
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/6/input-mini.txt"

  let parsedContent = parse contents
  print $ parsedContent

  print $ part1 parsedContent
  print $ part2 parsedContent
