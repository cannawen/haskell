import Data.Function ((&))
import Data.List.Split (splitOn, splitWhen)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.List.Split.Internals as T

parse1 input = lines input
  & map (splitOn " ")
  & map (filter (/= ""))
  & transpose
  & map reverse
  & map (\mathArray -> (head mathArray, tail mathArray))
  & map (\(operator, numbers) -> (if operator == "+" then sum else product, map read numbers))

part1 input = input
  & map calculate
  & sum

  where calculate (operator, numbers) = operator numbers

parse2 input = zip operations numbers
  where operations = filter (\c -> c == '*' || c == '+') (last $ lines input)
        numbers =
          input
          & lines
          & init
          & transpose
          & map T.pack
          & map T.strip
          & map T.unpack
          & splitWhen (=="")
          & map (map read)

part2 input = input
  & map (\(operation, numbers) -> if operation == '+' then sum numbers else product numbers)
  & sum

--     https://adventofcode.com/2025/day/6
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/6/input.txt"

  parse1 contents & part1 & print
  parse2 contents & part2 & print
