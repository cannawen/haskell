
import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Ord
import Data.Time
import Control.Monad

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

data Rect = Rect
    { xMin :: Int
    , yMin :: Int
    , xMax :: Int
    , yMax :: Int
    } deriving (Eq, Show)

instance Ord Rect where
    compare = comparing size

size :: Rect -> Int
size s = succ (xMax s - xMin s) * succ (yMax s - yMin s)

makeRect :: Point -> Point -> Rect
makeRect p1 p2 =
    Rect {
        xMin = min (x p1) (x p2),
        yMin = min (y p1) (y p2),
        xMax = max (x p1) (x p2),
        yMax = max (y p1) (y p2)
    }

borderPointsInRect :: Rect -> ([Point], Rect)
borderPointsInRect s =
    ([Point (xMin s) (yMin s), Point (xMin s) (yMax s), Point (xMax s) (yMax s), Point (xMax s) (yMin s)]
    & shapeFromPoints
    & map pointsFromSegment
    & concat
    , s)

cornerPointsInRect :: Rect -> ([Point], Rect)
cornerPointsInRect s =  (
    [
        Point (xMin s) (yMin s),
        Point (xMin s) (yMax s),
        Point (xMax s) (yMax s),
        Point (xMax s) (yMin s)
    ], s)

parse :: String -> [Point]
parse input =
    map (((\arr -> Point (head arr) (last arr)) . (map read)) . splitOn ",") (lines input)

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

isPointInShape :: Int -> ShapePoints -> ShapePoints -> Point -> Bool
isPointInShape bound shapeH shapeV point =
    Set.member point shapeH ||
    Set.member point shapeV ||
    odd (rayCast (createRay point bound) shapeV)

arePointsInShape maxYBound shapePointsHorizontal shapePointsVertical = map (isPointInShape maxYBound shapePointsHorizontal shapePointsVertical)

pointsFromSegment:: LineSegment -> [Point]
pointsFromSegment (p1, p2) =
    if isHorizontal (p1, p2)
        then [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]
        else [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]


part2 input =  insideRect
    where
        shape = shapeFromPoints input
        shapePointsHorizontal = Set.unions (map (Set.fromList . pointsFromSegment) (filter isHorizontal shape))
        shapePointsVertical = Set.unions (map (Set.fromList . pointsFromSegment) (filter (not . isHorizontal) shape))

        -- insidePoints = Set.fromList [Point x y | x <- [0.. maxXBound], y <- [0..maxYBound], isPointInShape (Point x y) maxYBound shapePointsHorizontal shapePointsVertical]

        insideRect = [makeRect p1 p2 | p1 <- input, p2 <- input, p1 < p2]
            & sortBy (comparing Down)
            & map cornerPointsInRect
            & filter (\(rectanglePoints, _) -> and (arePointsInShape maxYBound shapePointsHorizontal shapePointsVertical rectanglePoints))
            & map (\(_, rect) -> borderPointsInRect rect)
            & find (\(points, rect) -> and (arePointsInShape maxYBound shapePointsHorizontal shapePointsVertical points))

        maxXBound = map x input & maximum & succ
        maxYBound = map y input & maximum & succ

main = do
    now <- getCurrentTime
    putStrLn (formatTime defaultTimeLocale "%A, %B %e, %Y - %H:%M:%S" now)

    contents <- readFile "app/aoc/2025/9/input.txt"

    print $ part2 $ parse contents

    done <- getCurrentTime
    putStrLn (formatTime defaultTimeLocale "%A, %B %e, %Y - %H:%M:%S" done)
