import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord)

instance Num Point where
    (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
    (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger n = Point (fromInteger n) (fromInteger n)

instance Show Point where
    show (Point {x=x, y=y}) = "(" ++ show x ++ "," ++ show y ++ ")"

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (last arr))

-- Part 1 ----------------------------------------------------------------------------------------------------------------

squareSize p1 p2 = (succ ((x p2) - (x p1))) * (succ ((y p2) - (y p1)))

part1 input = [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (\(p1, p2) -> squareSize p1 p2)
    & sort
    & last

-- Part 2 (Brute force) ----------------------------------------------------------------------------------------------------------------

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

-- Part 2 (Ray Casting) ----------------------------------------------------------------------------------------------------------------

crossings :: Int -> Point -> Point -> Set.Set Point -> Set.Set Point -> Int
crossings count p dir shape bounds
  | outOfBounds = count
  | countIncrement = crossings (succ count) (p + dir) dir shape bounds
  | otherwise = crossings count (p + dir) dir shape bounds
  where
      outOfBounds = Set.member p bounds
      countIncrement = Set.member p shape

isSquareInside (p1, p2) shape bounds = allCrossings & map odd & foldl1' (&&)
    where xMin = min (x p1) (x p2)
          xMax = max (x p1) (x p2)
          xArray = [xMin .. xMax]

          yMin = min (y p1) (y p2)
          yMax = max (y p1) (y p2)
          yArray = [yMin .. yMax]

          topPoints = [Point x yMin | x <- xArray]
          rightPoints = [Point xMax y | y <- yArray]
          bottomPoints = [Point x yMax | x <- xArray]
          leftPoints = [Point xMin y | y <- yArray]

          topCrossings = topPoints & map (\p -> crossings 0 p (Point 0 (-1)) shape bounds)
          rightCrossings = rightPoints & map (\p -> crossings 0 p (Point 1 0) shape bounds)
          bottomCrossings = bottomPoints & map (\p -> crossings 0 p (Point 0 1) shape bounds)
          leftCrossings = topPoints & map (\p -> crossings 0 p (Point (-1) 0) shape bounds)

          allCrossings = topCrossings ++ rightCrossings ++ bottomCrossings ++ leftCrossings

getStraightPointsBetween (p1, p2) = [Point x y | x <- [xMin, xMax], y <- [yMin .. yMax]] ++ [Point x y | x <- [xMin .. xMax], y <- [yMin, yMax]] & Set.fromList
    where xMin = min (x p1) (x p2)
          xMax = max (x p1) (x p2)
          yMin = min (y p1) (y p2)
          yMax = max (y p1) (y p2)

part2 input = 
    sortedSquares &
    filter (\square -> isSquareInside square shapePoints boundsPoints)

    where sortedSquares =
            [(squareSize p1 p2, p1, p2) | p1 <- input, p2 <- input, p1 < p2]
            & sort
            & reverse
            & map (\(s, p1, p2) -> (p1, p2))
          shapePoints =
            zip input (rotate input)
            & map getStraightPointsBetween
            & foldl1' Set.union
          inputMaxX =
            input
            & map x
            & sort
            & last
          inputMaxY =
            input
            & map y
            & sort
            & last
          boundsPoints = getStraightPointsBetween (Point (-2) (-2), Point (2 + inputMaxX) (2 + inputMaxY))

main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    -- print $ part1 $ parse contents
    print $ part2 $ parse contents
