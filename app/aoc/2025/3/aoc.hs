import Data.List.Split (splitOn)
import Data.Function ((&))
import Data.Char (digitToInt)


parse input = lines input
  & map (map digitToInt)



--     https://adventofcode.com/2025/day/3
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/3/input-mini.txt"

  let parsedContent = parse contents
  print parsedContent

  -- print (part1 parsedContent)
  -- print (part2 parsedContent)
