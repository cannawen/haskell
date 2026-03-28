import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
    { x :: Double
    , y :: Double
    } deriving (Show, Eq, Ord)

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (last arr))

squareSize p1 p2 = (succ ((x p2) - (x p1))) * (succ ((y p2) - (y p1)))

part1 input = [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (\(p1, p2) -> squareSize p1 p2)
    & sort
    & last


pointSet p1 p2 = Set.fromList [Point x y | x <- [(x p1) .. (x p2)], y <- [(min (y p1) (y p2)) .. (max (y p1) (y p2))]]

part2 input = [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
              & map (\(p1, p2) -> pointSet p1 p2)
              & filter (\pointSet -> Set.disjoint pointSet (Set.fromList input))
              & map Set.size
              & sort
              & last
          
          

main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    print $ part1 $ parse contents
    print $ part2 $ parse contents
