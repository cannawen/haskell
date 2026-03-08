import Data.Function ((&))
import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

parse input = (parseRanges ranges, parseFoods $ tail foods)
  where splitIndex = fromMaybe 0 (elemIndex "" (lines input))
        (ranges, foods) = splitAt splitIndex (lines input)

parseRanges :: [String] -> Set.Set Int
parseRanges ranges = ranges
  & map (break (== '-'))
  & concatMap (\(start, end) -> [read start..read $ tail end])
  & Set.fromList

parseFoods :: [String] -> Set.Set Int
parseFoods foods = map read foods & Set.fromList

part1 (validSet, foodSet) = Set.intersection validSet foodSet & Set.size

part2 input = input

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input.txt"

  let parsedContent = parse contents
  print parsedContent

  print $ part1 parsedContent
  print $ part2 parsedContent
