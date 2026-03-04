import Data.List.Split (splitOn)
import Data.Function ((&))

parse :: [Char] -> [[Int]]
parse input = map (map read . splitOn "-") (splitOn "," input)

part1 :: [[Int]] -> Int
part1 input = input
  & concatMap (\[from, to] -> [from..to])
  & map show
  & map (\numberString -> splitAt (length numberString `div` 2) numberString)
  & filter (\(firstHalf, lastHalf) -> firstHalf == lastHalf)
  & map (\(halfNum, _) -> halfNum ++ halfNum)
  & map read
  & sum

hasRepeatingDigits :: [Char] -> Bool
hasRepeatingDigits string = -- "foobar"
  let stringLength = length string -- 6
      possibleSubstrLengths = [1..stringLength `div` 2] -- [1, 2, 3]
      possibleSubstrings = map (\length -> fst $ splitAt length string) possibleSubstrLengths -- ["f", "fo", "foo"]
      possibleSubstringRepeated = map (\str -> take stringLength (cycle str)) possibleSubstrings -- ["ffffff", "fofofo", "foofoo"]
  in 
    elem string possibleSubstringRepeated


part2 input = input
  & concatMap (\[from, to] -> [from..to])
  & map show
  & filter hasRepeatingDigits
  & map read
  & sum


--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/2/input.txt"

    let parsedContent = parse contents
    print parsedContent

    print (part1 parsedContent)
    print (part2 parsedContent)
