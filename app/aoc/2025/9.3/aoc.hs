import Data.Function ( (&) )
import Data.List.Split ( splitOn )
import Data.List ( foldl', find, sort, sortBy )
import Data.Ord ( comparing, Down(Down) )
import qualified Data.Map as Map
import qualified Data.Set as Set

data Point = 
    Point
    { 
        x :: Int, 
        y :: Int
    } deriving (Eq, Ord)

type LineSegment = (Point, Point)

isVertical :: LineSegment -> Bool
isVertical (p1, p2) = y p1 == y p2

data Rect = 
    Rect
    { 
        xMin :: Int, 
        yMin :: Int, 
        xMax :: Int, 
        yMax :: Int
    } deriving (Eq, Show)

instance Ord Rect where
    compare = comparing size

size :: Rect -> Int
size rect = succ (xMax rect - xMin rect) * succ (yMax rect - yMin rect)

makeRect :: [Point] -> Rect
makeRect pts =
    Rect 
    {
        xMin = map x pts & minimum, 
        yMin = map y pts & minimum,
        xMax = map x pts & maximum,
        yMax = map y pts & maximum
    }

cornerPointsInRect :: Rect -> [Point]
cornerPointsInRect rect =
    [
        Point (xMin rect) (yMin rect),
        Point (xMin rect) (yMax rect),
        Point (xMax rect) (yMax rect),
        Point (xMax rect) (yMin rect)
    ]

borderPointsInRect :: Rect -> [Point]
borderPointsInRect rect =
    rect
    & cornerPointsInRect
    & lineSegmentsFromConsecutivePoints
    & concatMap intermediaryPoints

intermediaryPoints :: LineSegment -> [Point]
intermediaryPoints (p1, p2) = do
    x <- [min (x p1) (x p2) .. max (x p1) (x p2)]
    y <- [min (y p1) (y p2) .. max (y p1) (y p2)]
    return (Point x y)

lineSegmentsFromConsecutivePoints :: [Point] -> [LineSegment]
lineSegmentsFromConsecutivePoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

type Row = Int

type Column = Int

verticalPointsForRayTracing :: LineSegment -> [Point]
verticalPointsForRayTracing (p1, p2) =
    if isVertical (p1, p2)
        then [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]
        else []

pointInsideShape :: Point -> Map.Map Row [Column] -> Set.Set Point -> Bool
pointInsideShape point shape borderPoints = 
    Set.member point borderPoints || isPointInEncodedShape
    where isPointInEncodedShape =
            maybe 
            False 
            (\column -> 
                column
                & takeWhile (> y point)
                & length
                & odd)
            (Map.lookup (x point) shape)

part2 :: [Point] -> Maybe Rect
part2 input =
    rects
    & filter (all (\p -> pointInsideShape p encodedShape shapeBorder) . cornerPointsInRect)
    & find (all (\p -> pointInsideShape p encodedShape shapeBorder) . borderPointsInRect)
    where
        rects = 
            [[p1, p2] | p1 <- input, p2 <- input, p1 < p2]
            & map makeRect
            & sortBy (comparing Down)
        encodedShape =
            input
            & lineSegmentsFromConsecutivePoints
            & concatMap verticalPointsForRayTracing
            & sort
            & foldl' 
            (\memo point ->
                if fst (head memo) == x point
                then (x point, y point : snd (head memo)) : tail memo
                else (x point, [y point]) : memo) 
            [(0,[])]
            & Map.fromList
        shapeBorder = 
            input
            & lineSegmentsFromConsecutivePoints 
            & concatMap intermediaryPoints
            & Set.fromList

parse :: String -> [Point]
parse input =
    input
    & lines
    & map (splitOn ",")
    & map (map read)
    & map (\arr -> Point (head arr) (last arr))

main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/9/input.txt"
    print $ size <$> part2 (parse contents)
