import Data.Function ((&))

parse input = lines input

part1 input = matrix
  where rowSize = length input
        columnSize =  head input & length
        matrix = [(x,y) | x <- [0..pred rowSize], y <- [0..pred columnSize]]

part2 input = input

--     https://adventofcode.com/2025/day/4
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/4/input-mini.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  print (part2 parsedContent)
