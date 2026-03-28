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

notBetween a b c = a < (min b c) || a > (max b c)

createSet points p1 p2 = points
    & filter (\p -> if x p == x p1 || x p == x p2
                    then notBetween (y p) (y p1) (y p2)
                    else if y p == y p1 || y p == y p2
                    then notBetween (x p) (x p1) (x p2)
                    else True)
    & Set.fromList

isValid :: [Point] -> Point -> Point -> Bool
isValid points p1 p2 = points
    & find (\p -> 
        ((x p) > min (x p1) (x p2) && (x p) < max (x p1) (x p2)) 
        && 
        ((y p) > min (y p1) (y p2) && (y p) < max (y p1) (y p2)))
    & (== Nothing)

shape = [Point 1 1, 
         Point 4 1, 
         Point 3 2, 
         Point 3 3, 
         Point 4 3, 
         Point 4 4, 
         Point 1 4]

part2 input = [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
              & filter (\(p1, p2) -> isValid input p1 p2)
              & map (uncurry pointSet)
              & map Set.size
              & maximum
          
main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    print $ part1 $ parse contents
    print $ part2 $ parse contents
