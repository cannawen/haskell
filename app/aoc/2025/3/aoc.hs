import Data.Function ((&))
import Data.Char (digitToInt)
import Data.List (elemIndex)  
import Data.Maybe (fromMaybe)

parse input = lines input
  & map (map digitToInt)

findJoltage intArray = maxInt * 10 + onesDigit
  where maxInt = maximum (init intArray)
        maxIndex = fromMaybe (error "") (elemIndex maxInt intArray)
        onesDigit = maximum (drop (succ maxIndex) intArray)

foldFn memo newElement = (contractedArray, snd memo ++ [expandedArray !! index])
  where expandedArray = fst memo ++ [newElement]
        index = fromMaybe (error "") (elemIndex (maximum expandedArray) expandedArray)
        contractedArray = drop (succ index) expandedArray

find12Joltage intArray = foldl foldFn (dropLast 12 intArray,[]) (reverse (take 12 (reverse intArray)))
  & snd
  & concatMap show
  & read
  where dropLast last = reverse . drop last . reverse

part2 content = content
  & map find12Joltage
  & sum

part1 content = content
  & map findJoltage
  & sum

--     https://adventofcode.com/2025/day/3
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/3/input.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  print (part2 parsedContent)
