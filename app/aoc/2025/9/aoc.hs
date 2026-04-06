import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord)

instance Show Point where
    show (Point {x=x, y=y}) = "(" ++ show x ++ "," ++ show y ++ ")"

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

rotate arr = tail arr ++ [head arr]

shouldSwitch input set x y = Set.member (Point x y) set -- && (not $ elem (Point x y) input)

part2_brute input = shapeH
    where outlineSet = 
            zip input (rotate input)
            & map (\(p1, p2) -> [Point x y | x <- [min (x p1) (x p2) .. max (x p1) (x p2)], y <- [min (y p1) (y p2) .. max (y p1) (y p2)]])
            & concat
            & Set.fromList
          horizontalSet = 
            zip input (rotate input)
            & filter (\(p1, p2) -> x p1 == x p2)
            & map (\(p1, p2) -> [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]])
            & concat
            & Set.fromList
          grid = [Point x y | x <- [0 .. input & map x & maximum], y <- [0 .. input & map y & maximum]]
          shapeH = 
            map 
            (\y -> 
                foldl' 
                (\memo x -> if shouldSwitch input horizontalSet x y then memo ++ [not $ last memo] else memo ++ [last memo]
                )
                [False]
                [0 .. input & map x & maximum]
                & tail
            ) 
            [0 .. input & map y & maximum]

isSquareInside (p1, p2) bounds = True

getStraightPointsBetween (p1, p2) = [(x, y) | x <- [xMin, xMax], y <- [yMin .. yMax]] ++ [(x, y) | x <- [xMin .. xMax], y <- [yMin, yMax]]
    where xMin = min (x p1) (x p2)
          xMax = max (x p1) (x p2)
          yMin = min (y p1) (y p2)
          yMax = max (y p1) (y p2)


part2 input = sortedSquares
    where sortedSquares = 
            [(squareSize p1 p2, p1, p2) | p1 <- input, p2 <- input, p1 < p2]
            & sort
            & map (\(s, p1, p2) -> (p1, p2))
            & reverse
          outlineSet = 
            zip input (rotate input)
            & map getStraightPointsBetween
            & concat
            & Set.fromList
        
          
main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    -- print $ part1 $ parse contents
    print $ part2 $ parse contents
