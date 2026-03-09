import Data.Function ((&))
import Data.List.Split (splitOn, splitWhen)
import Data.List (elemIndices)
import qualified Data.Text as T
import qualified Data.List.Split.Internals as T

parse input = lines input

modifyCurrentRow prevRow row = 
  let findHit = map (\(prev, curr) -> if prev == 'S' && curr == '.' then 'S' else 
                                     if prev == 'S' && curr == '^' then 'h' else 
                                     curr) (zip prevRow row)
      hIndices = elemIndices 'h' findHit
      newSIndices = concatMap (\i -> [i-1, i+1]) hIndices
      newRow = map (\(i, curr) -> if elem i newSIndices then 'S' else curr) (zip [0..] findHit)

  in newRow

part1 input = scanl1 modifyCurrentRow input -- & concat & filter (=='h') & length

part2 input = "pt2"

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/7/input-mini.txt"

  parse contents & part1 & print
  parse contents & part2 & print
