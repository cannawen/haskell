import Data.Function ((&))
import qualified Data.Set as Set

parse input = lines input

get x y input =
    if x < 0 || y < 0 || x >= (head input & length) || y >= length input
      then Nothing
      else Just (input !! y !! x)

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

getPt2 x y input removedPaperIndices = if Set.member (x, y) removedPaperIndices then Just '.' else get x y input

pt2Recursive originalGrid previouslyRemovedIndices =
  if newlyRemoved == Set.empty 
    then previouslyRemovedIndices
    else pt2Recursive originalGrid (Set.union newlyRemoved previouslyRemovedIndices)
  where newlyRemoved = 
          foldl (\memo (x, y) ->
                  let neighborIndexMatrix = [(nx, ny) | nx <- [(x-1)..(x+1)], ny <- [(y-1)..(y+1)], (nx, ny) /= (x,y)]
                      neighborValues = map (\(x, y) -> getPt2 x y originalGrid previouslyRemovedIndices) neighborIndexMatrix
                      canRemovePaper = getPt2 x y originalGrid previouslyRemovedIndices == Just '@' && (filter (== Just '@') neighborValues & length) < 4
                  in if canRemovePaper then Set.insert (x, y) memo else memo)
                Set.empty 
                [(x,y) | x <- [0..pred (head originalGrid & length)], y <- [0..pred (length originalGrid)]]

part2 input =
  pt2Recursive input Set.empty
  & Set.size
  & show

--     https://adventofcode.com/2025/day/4
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/4/input.txt"

  let parsedContent = parse contents

  print $ part1 parsedContent
  print $ part2 parsedContent
