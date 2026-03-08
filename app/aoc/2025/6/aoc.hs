import Data.Function ((&))
import Data.List.Split (splitOn)
import Data.List (transpose)

parse :: String -> [(Int -> Int -> Int, [Int])]
parse input = (lines input)
  & map (splitOn " ")
  & map (filter (/= ""))
  & transpose
  & map reverse
  & map (\mathArray -> (head mathArray, tail mathArray))
  & map (\(operator, numbers) -> (if operator == "+" then (+) else (*), map read numbers))

part1 input = input
  & map calculate
  & sum

  where calculate (operator, numbers) = foldl1 operator numbers

part2 input = "pt2"

--     https://adventofcode.com/2025/day/6
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/6/input.txt"

  let parsedContent = parse contents
  -- print $ parsedContent

  print $ part1 parsedContent
  print $ part2 parsedContent
