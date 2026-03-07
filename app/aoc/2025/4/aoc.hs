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

----------------------------------------------

get x y input =
    if x < 0 || y < 0 || x >= (head input & length) || y >= length input
      then Nothing
      else Just (input !! y !! x)

-- update x y input = 

matrix input = [(x,y) | x <- [0..pred (head input & length)], y <- [0..pred (length input)]]

movablePaperIndices input = foldl (\memo (x, y) ->
  let neighborIndexMatrix = [(nx,ny) | nx <- [(x-1)..(x+1)], ny <- [(y-1)..(y+1)], (nx, ny) /= (x,y)]
      neighborValues = map (\(x,y) -> get x y input) neighborIndexMatrix
      canRemovePaper = get x y input  == Just '@' && (filter (== Just '@') neighborValues & length) < 4
  in if canRemovePaper then (x, y):memo else memo) [] (matrix input)


part2 input = 
  movablePaperIndices input
  & length
  & show
        -- removePaper = foldl (\memo (x,y) -> if elem (x,y) movablePaperIndices then set x y else longInput) longInput matrix

--     https://adventofcode.com/2025/day/4
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/4/input-mini.txt"

  let parsedContent = parse contents
  print parsedContent

  -- print (part1 parsedContent)
  print (part2 parsedContent)
