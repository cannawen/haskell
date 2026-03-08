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
        parseFoods = map read

part1 (validRanges, food) = filter (inRange validRanges) food & length & show
  where inRange validRanges food = validRanges
          & any (\range -> food >= fst range && food <= snd range)

merge :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
merge range1 range2       -- r1's start is always before or equal to r2's start
  | e1 < s2 = [r1, r2]    -- fully outside    (...r1...)  (,,,r2,,,) 
  | e1 == s2 = [(s1, e2)] -- touching         (...r1...)(,,,r2,,,)
  | e2 < e1 = [r1]        -- fully inside     (...r1...(;;;r2;;;)...)
  | s2 < e1 = [(s1, e2)]  -- partially inside (...r1...(;;;r2;;;),,,)
  | otherwise = [r1, r2]  -- should never happen, I think?
  where
      [r1, r2] = sort [range1, range2]
      s1 = fst r1
      e1 = snd r1
      s2 = fst r2
      e2 = snd r2

part2 (ranges, _) = foldl (\memo range -> init memo ++ merge range (last memo) ) [head rangeStartToEnd] (tail rangeStartToEnd)
  & map (\(start, end) -> end - start + 1)
  & sum

  where rangeStartToEnd = sort ranges
        rangesEndToStart = map swap ranges & sort

--     https://adventofcode.com/2025/day/5
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/5/input.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  print $ part2 parsedContent
