import Data.Function ((&))
import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

parse input = (parseRanges ranges, parseFoods $ tail foods)
  where splitIndex = fromMaybe 0 (elemIndex "" (lines input))
        (ranges, foods) = splitAt splitIndex (lines input)

parseRanges :: [String] -> [(Int, Int)]
parseRanges ranges = ranges
  & map (break (== '-'))
  & map (\(start, end) -> (read start, read $ tail end))

parseFoods :: [String] -> [Int]
parseFoods foods = map read foods

part1 input = input

part2 input = input

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input-mini.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  print $ part2 parsedContent
