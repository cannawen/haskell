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

dropElems:: Int -> Int -> [Int] -> [Int]
dropElems first last array =
  array
  & drop first
  & reverse
  & drop last
  & reverse

findMaximum intArray = (maxInt, maxIndex)
  where maxInt = maximum intArray
        maxIndex = fromMaybe (error "") (elemIndex maxInt intArray)

find12Joltage intArray = 
  (fst firstDigit) * 10^11 
  + (fst secondDigit) * 10^10
  + (fst thirdDigit) * 10^9
  + (fst fourthDigit) * 10^8
  + (fst fifthDigit) * 10^7
  + (fst sixthDigit) * 10^6
  + (fst seventhDigit) * 10^5
  + (fst eighthDigit) * 10^4
  + (fst ninthDigit) * 10^3
  + (fst tenthDigit) * 10^2
  + (fst eleventhDigit) * 10^1
  + (fst twelfthDigit) * 10^0

  where 
    firstDigit = findMaximum (dropElems 0 11 intArray)
    secondDigit =  findMaximum (dropElems (snd firstDigit) 10 intArray)
    thirdDigit =  findMaximum (dropElems (snd secondDigit) 9 intArray)
    fourthDigit = findMaximum (dropElems (snd thirdDigit) 8 intArray)
    fifthDigit    = findMaximum (dropElems (snd fourthDigit) 7 intArray)
    sixthDigit    = findMaximum (dropElems (snd fifthDigit) 6 intArray)
    seventhDigit  = findMaximum (dropElems (snd sixthDigit) 5 intArray)
    eighthDigit   = findMaximum (dropElems (snd seventhDigit) 4 intArray)
    ninthDigit    = findMaximum (dropElems (snd eighthDigit) 3 intArray)
    tenthDigit    = findMaximum (dropElems (snd ninthDigit) 2 intArray)
    eleventhDigit = findMaximum (dropElems (snd tenthDigit) 1 intArray)
    twelfthDigit  = findMaximum (dropElems (snd eleventhDigit) 0 intArray)

part2 content = content
  & map find12Joltage

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
