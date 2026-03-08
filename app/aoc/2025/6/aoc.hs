import Data.Function ((&))

parse input = input

part1 input = input

part2 input = input

--     https://adventofcode.com/2025/day/6
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/6/input.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  print $ part2 parsedContent
