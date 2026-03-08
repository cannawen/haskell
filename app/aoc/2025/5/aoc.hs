import Data.Function ((&))
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Tuple (swap)

parse input = (parseRanges ranges, parseFoods $ tail foods)
  where splitIndex = fromMaybe 0 (elemIndex "" (lines input))
        (ranges, foods) = splitAt splitIndex (lines input)

        parseRanges :: [String] -> [(Int, Int)]
        parseRanges ranges = ranges
          & map (break (== '-'))
          & map (\(start, end) -> (read start, read $ tail end))

        parseFoods :: [String] -> [Int]
        parseFoods foods = map read foods

part1 (validRanges, food) = filter (\food -> inRange food validRanges) food & length & show
  where inRange food validRanges = validRanges
          & any (\range -> food >= fst range && food <= snd range)

merge :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
merge range1 range2 = if e1 < s2 then [r1, r2] else 
                      if e1 == s2 then [(s1, e2)] else 
                      if e2 < e1 then [r1] else 
                      if s2 < e1 then [(s1, e2)] else
                      [r1, r2]
  where [r1, r2] = sort [range1, range2]
        s1 = fst r1
        e1 = snd r1
        s2 = fst r2
        e2 = snd r2

part2 (ranges, _) = foldl (\memo range -> concatMap (\mem -> merge range mem) memo) [head rangeStartToEnd] (tail rangeStartToEnd)

  where rangeStartToEnd = sort ranges
        rangesEndToStart = map swap ranges & sort

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input-mini.txt"

  let parsedContent = parse contents
  print parsedContent

  print $ part1 parsedContent
  print $ part2 parsedContent
