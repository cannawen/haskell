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

findLeftmostMaximum :: [Int] -> (Int, [Int])
findLeftmostMaximum integerList = (maxInt, remainingIntegers)
  where maxInt = maximum integerList
        maxIndex = fromMaybe (error "") (elemIndex maxInt integerList)
        remainingIntegers = drop (succ maxIndex) integerList

dropLast n = reverse . drop n . reverse

findLeftmostMaximumWithDigits :: Int -> [Int] -> (Int, [Int])
findLeftmostMaximumWithDigits 0 _ = (0, [])
findLeftmostMaximumWithDigits n integerList = ((fst result) * (10 ^ (pred n)) , snd result)
  where result = findLeftmostMaximum (dropLast n integerList)

part2 content = content
  & map (\intList -> ( map (\d -> findLeftmostMaximumWithDigits d intList ) (reverse [0..12])))
  & map (map fst)
  & map sum

part1 content = content
  & map findJoltage
  & sum

--     https://adventofcode.com/2025/day/3
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/3/input-mini.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  print (part2 parsedContent)
