import Data.List.Split (splitOn)
import Data.Function ((&))
import Data.Char (digitToInt)
import Data.List (elemIndex, findIndex)

import Data.Maybe (fromMaybe)
parse input = lines input
  & map (map digitToInt)

findJoltage intArray = maxInt * 10 + onesDigit
  where maxInt = maximum (init intArray)
        maxIndex = fromMaybe (error "") (elemIndex maxInt intArray)
        onesDigit = maximum (drop (succ maxIndex) intArray)

part1 content = content
  & map findJoltage
  & sum

--     https://adventofcode.com/2025/day/3
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/3/input.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  -- print (part2 parsedContent)
