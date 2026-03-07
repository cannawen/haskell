import Data.Function ((&))
import Data.Maybe (fromMaybe)

parse input = lines input

part1 :: [String] -> String
part1 input =
  foldl (\memo (x, y) ->
          let neighborIndexMatrix = [(nx,ny) | nx <- [(x-1)..(x+1)], ny <- [(y-1)..(y+1)], (nx, ny) /= (x,y)]
              neighborValues = map (\(x,y) -> get x y) neighborIndexMatrix
              canFitForklift = get x y  == Just '@' && (filter (== Just '@') neighborValues & length) < 4
          in if canFitForklift then succ memo else memo
        ) 0 matrix
  & show

  where rowNum = length input
        rowSize =  head input & length
        longInput = concat input
        get x y =
          if x < 0 || y < 0 || x >= rowSize || y >= rowNum
            then Nothing
            else Just (longInput !! (y * rowSize + x))
        matrix = [(x,y) | x <- [0..pred rowSize], y <- [0..pred rowNum]]

part2 input = input

--     https://adventofcode.com/2025/day/4
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/4/input.txt"

  let parsedContent = parse contents

  print (part1 parsedContent)
  print (part2 parsedContent)
