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

findMaximum intArray = maxIndex
  where maxInt = maximum intArray
        maxIndex = fromMaybe (error "") (elemIndex maxInt intArray)

find12Joltage intArray = 
 firstDigit * 10^11 
  + secondDigit * 10^10
  + thirdDigit * 10^9
  + fourthDigit * 10^8
  + fifthDigit * 10^7
  + sixthDigit * 10^6
  + seventhDigit * 10^5
  + eighthDigit * 10^4
  + ninthDigit * 10^3
  + tenthDigit * 10^2
  + eleventhDigit * 10^1
  + twelfthDigit * 10^0

  where 
    firstDigit = findMaximum (dropElems 0 11 intArray)
    secondDigit =  findMaximum (dropElems firstDigit 10 intArray)
    thirdDigit =  findMaximum (dropElems secondDigit 9 intArray)
    fourthDigit = findMaximum (dropElems thirdDigit 8 intArray)
    fifthDigit    = findMaximum (dropElems fourthDigit 7 intArray)
    sixthDigit    = findMaximum (dropElems fifthDigit 6 intArray)
    seventhDigit  = findMaximum (dropElems sixthDigit 5 intArray)
    eighthDigit   = findMaximum (dropElems seventhDigit 4 intArray)
    ninthDigit    = findMaximum (dropElems eighthDigit 3 intArray)
    tenthDigit    = findMaximum (dropElems ninthDigit 2 intArray)
    eleventhDigit = findMaximum (dropElems tenthDigit 1 intArray)
    twelfthDigit  = findMaximum (dropElems eleventhDigit 0 intArray)

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
