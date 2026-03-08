import Data.Function ((&))
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


inRange food validRanges = validRanges
  & any (\range -> food >= fst range && food <= snd range)

part1 (validRanges, foodSet) = filter (\food -> inRange food validRanges) foodSet & length & show

part2 input = input

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  -- print $ part2 parsedContent
