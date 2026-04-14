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

squareSize p1 p2 =  succ ((p1 - p2) & x & abs) * (succ ((p1 - p2) & y & abs))

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

type LineSegment = (Point, Point)
type Square = (Point, Point)

topLeft :: Square -> Point
topLeft square = getPointOnSquare square min min

topRight :: Square -> Point
topRight square = getPointOnSquare square min max

bottomLeft :: Square -> Point
bottomLeft square = getPointOnSquare square max min

bottomRight :: Square -> Point
bottomRight square = getPointOnSquare square max max

getPointOnSquare :: Square -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Point
getPointOnSquare (p1, p2) xFn yFn = Point (xFn (x p1) (x p2)) (yFn (y p1) (y p2))

lineToRight :: Point -> Int -> LineSegment
lineToRight point xMax = (Point (x point) (y point), Point xMax (y point))

allPointsBordering :: Square -> [Point]
allPointsBordering (p1, p2) = 
    [Point xMin y | y <- [yMin .. yMax]]
    ++ [Point xMax y | y <- [yMin .. yMax]]
    ++ [Point x yMin | x <- [xMin .. xMax]]
    ++ [Point x yMax | x <- [xMin .. xMax]]
  where 
    xMin = min (x p1) (x p2)
    xMax = max (x p1) (x p2)
    yMin = min (y p1) (y p2)
    yMax = max (y p1) (y p2)

isSquareInside :: Square -> [LineSegment] -> Int -> Bool
isSquareInside square shape xMax = foldl' (\memo p -> memo && isInShape p xMax shape) True (allPointsBordering square)

isPointOnBounds :: Point -> [LineSegment] -> Bool
isPointOnBounds p bounds = foldl' (\m (l1, l2) -> m
    || (x l1 == x l2 && x p == x l1 && y p <= max (y l1) (y l2) && y p >= min (y l1) (y l2))
    || (y l1 == y l2 && y p == y l1 && x p <= max (x l1) (x l2) && x p >= min (x l1) (x l2))
    ) False bounds

intersection :: LineSegment -> LineSegment -> Bool
intersection horizontal line =
  if x (fst line) == x (snd line)
    then verticalIntersection horizontal (lowToHighVertical line)
    else horizontalIntersection horizontal (lowToHighHorizontal line)
  where 
    lowToHighVertical (p1, p2) = (Point (x p1) (min (y p1) (y p2)), Point (x p1) (max (y p1) (y p2)))
    lowToHighHorizontal (p1, p2) = (Point (min (x p1) (x p2)) (y p1), Point (max (x p1) (x p2)) (y p1))

    verticalIntersection horizontal vertical = 
        x (fst horizontal) <= x (fst vertical) 
        && x (fst vertical) <= x (snd horizontal) 
        && y (fst vertical) <= y (fst horizontal)
        && y (fst horizontal) <= y (snd vertical)
    horizontalIntersection horizontal line = 
        y (fst horizontal) == y (fst line)
        && oneDIntersection (x (fst horizontal)) (x (snd horizontal)) (x (fst line)) (x (snd line))
    oneDIntersection min1 max1 min2 max2 =
        (min1 <= max2 && max2 <= max1)
        || (min1 <= min2 && min2 <= max1)
        || (min2 <= min1 && max2 >= max1)
        || (min2 >= min1 && max2 <= max1)
        
numLinesCrossed :: LineSegment -> [LineSegment] -> Int
numLinesCrossed line shape = map (intersection line) shape & map (\b -> if b then 1 else 0) & sum

isOnBorder :: Point -> [LineSegment] -> Bool
isOnBorder p shape = 
    any (\(p1, p2) -> min (y p1) (y p2) <= y p && y p <= max (y p1) (y p2)) vertical
    || any (\(p1, p2) -> min (x p1) (x p2) <= x p && x p <= max (x p1) (x p2)) horizontal
    where
        vertical = filter (\(p1, p2) -> x p1 == x p2 && x p == x p1) shape
        horizontal = filter (\(p1, p2) -> y p1 == y p2 && y p == y p1) shape

isInShape :: Point -> Int -> [LineSegment] -> Bool
isInShape point xMax shape = isOnBorder point shape || odd (numLinesCrossed (lineToRight point xMax)  shape)

shapeCoords :: [Point] -> [LineSegment]
shapeCoords input = zip input (rotate input)

part2 input = 
    sortedSquares
    & filter (\square -> isSquareInside square (shapeCoords input) inputMaxX)

    where sortedSquares =
            [(squareSize p1 p2, p1, p2) | p1 <- input, p2 <- input, p1 < p2]
            & sort
            & reverse
            & map (\(s, p1, p2) -> (p1, p2))
          inputMaxX = map x input & maximum

main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    -- print $ part1 $ parse contents
    print $ part2 $ parse contents
