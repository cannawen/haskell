import Data.List.Split (splitOn)
import Data.Function ((&))

parse input = splitOn "," input -- "1-2,3-5"
  & map (map (read :: String->Int) . splitOn "-") -- [[1-2], [3-5]]
  & concatMap (\[from, to] -> [from..to]) -- [1, 2, 3, 4, 5]
  & map show -- ["1", "2", "3", "4", "5"]

part1 input = input -- array of "123123"
  & map (\numberString -> splitAt (length numberString `div` 2) numberString) -- ["123", "123"]
  & filter (\(firstHalf, lastHalf) -> firstHalf == lastHalf) -- "123" == "123" ?
  & map (\(halfNum, _) -> halfNum ++ halfNum) -- "123123"
  & map read -- 1234
  & sum

hasRepeatingDigits string = -- "1234512345"
  any (\chunkSize -> repeatsFirst chunkSize == string) validChunkSize
  where 
    n = length string -- 10
    validChunkSize = [x | x <- [1 .. div n 2], mod n x == 0] -- [1, 2, 5]
    repeatsFirst x = take n (cycle (take x string)) -- ["1111111111", "1212121212", "1234512345"]

part2 input = input
  & filter hasRepeatingDigits
  & map read
  & sum

--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/2/input.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  print (part2 parsedContent)
