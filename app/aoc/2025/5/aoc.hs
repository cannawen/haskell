import Data.Function ((&))
import qualified Data.Set as Set

parse input = lines input

part1 input = input

part2 input = input

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input-mini.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  print $ part2 parsedContent
