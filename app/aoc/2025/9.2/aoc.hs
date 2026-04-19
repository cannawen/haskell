
import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Ord

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

type LineSegment = (Point, Point)

isHorizontal :: LineSegment -> Bool
isHorizontal (p1, p2) = x p1 == x p2

type ShapePoints = Set.Set Point

data Square = Square
    { xMin :: Int
    , yMin :: Int
    , xMax :: Int
    , yMax :: Int
    } deriving (Eq, Show)

instance Ord Square where
    compare = comparing size

size :: Square -> Int
size s = (xMax s - xMin s) * (yMax s - yMin s)

makeSquare :: Point -> Point -> Square
makeSquare p1 p2 =
    Square {
        xMin = min (x p1) (x p2),
        yMin = min (y p1) (y p2),
        xMax = max (x p1) (x p2),
        yMax = max (y p1) (y p2)
    }

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (last arr))

shapeFromPoints :: [Point] -> [LineSegment]
shapeFromPoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

rayCast:: LineSegment -> ShapePoints -> Int
rayCast line shape = 
    foldl 
    (\memo p -> if Set.member p shape then memo + 1 else memo) 
    0 
    (pointsFromSegment line)

createRay :: Point -> Int -> LineSegment
createRay p maxY = (p, Point (x p) maxY)

isPointInShape :: Point -> Int -> ShapePoints -> ShapePoints -> Bool
isPointInShape point bound shapeH shapeV =
    Set.member point shapeH ||
    Set.member point shapeV ||
    odd (rayCast (createRay point bound) shapeV)

pointsFromSegment:: LineSegment -> [Point]
pointsFromSegment (p1, p2) =
    if isHorizontal (p1, p2)
        then [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]
        else [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]

part2 input = [(Point x y, isPointInShape (Point x y) maxYBound shapePointsHorizontal shapePointsVertical )| x <- [0.. maxXBound], y <- [0..maxYBound]]
    where
        shape = shapeFromPoints input
        shapePointsHorizontal = Set.unions (map (Set.fromList . pointsFromSegment) (filter isHorizontal shape))
        shapePointsVertical = Set.unions (map (Set.fromList . pointsFromSegment) (filter (not . isHorizontal) shape))
        maxXBound = map x input & maximum & succ
        maxYBound = map y input & maximum & succ

main = do
    contents <- readFile "app/aoc/2025/9/input-mini.txt"

    print $ part2 $ parse contents

-- exampleInput = parse <$> readFile "app/aoc/2025/9/input-mini.txt"
