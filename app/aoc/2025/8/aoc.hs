import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
  { x :: Double
  , y :: Double
  , z :: Double
  } deriving (Show, Eq)
type Circuit = Set.Set Point

parse :: String -> [Point]
parse input = 
    lines input
    & map (splitOn ",") 
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (arr !! 1) (arr !! 2))

distance p1 p2 = (x p1 - x p2)^2 + (y p1 - y p2)^2 + (z p1 - z p2)^2

-- part1 :: [Point]
part1 input = input
    where combinations = 
            [Set.fromList [p1, p2] | p1 <- input, p2 <- input, p1 /= p2]
            & Set.fromList
            & Set.map (\s -> Set.toList s)
            & Set.toList
            & sort
          distancesSquared = 
            map 
                (\[p1, p2] -> (distance p1 p2, p1, p2)) 
                combinations
            & sort
            & take 10
          final = foldl
            (\memo (_, p1, p2)  -> mergePointsIntoSets memo p1 p2)
            Set.empty
            distancesSquared

main = do
    contents <- readFile "app/aoc/2025/8/input-mini.txt"

    print $ part1 $ parse contents
