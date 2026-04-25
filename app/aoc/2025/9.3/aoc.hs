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

makeRect :: Point -> Point -> Rect
makeRect p1 p2 =
    Rect 
    {
        xMin = min (x p1) (x p2),
        yMin = min (y p1) (y p2),
        xMax = max (x p1) (x p2),
        yMax = max (y p1) (y p2)
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

verticalPointsForRayTracing :: LineSegment -> [Point]
verticalPointsForRayTracing (p1, p2) =
    if isVertical (p1, p2)
        then [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]
        else []

lineSegmentsFromConsecutivePoints :: [Point] -> [LineSegment]
lineSegmentsFromConsecutivePoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

sortedRects :: [Point] -> [Rect]
sortedRects points =
    [(p1, p2) | p1 <- points, p2 <- points, p1 < p2]
    & map (uncurry makeRect)
    & sortBy (comparing Down)

type Row = Int

type Column = Int

encodedShape :: [Point] -> Map.Map Row [Column]
encodedShape points = 
    points
    & lineSegmentsFromConsecutivePoints
    & concatMap verticalPointsForRayTracing
    & encode

encode :: [Point] -> Map.Map Row [Column]
encode shapeV = 
    shapeV
    & sort 
    & foldl' 
      (\memo point ->
          if fst (head memo) == x point
          then (x point, y point : snd (head memo)) : tail memo
          else (x point, [y point]) : memo) 
      [(0,[])]
    & Map.fromList

shapePoints :: [LineSegment] -> Set.Set Point
shapePoints shape = 
    shape 
    & concatMap intermediaryPoints
    & Set.fromList

isPointInEncodedShape :: Point -> Map.Map Row [Column] -> Bool
isPointInEncodedShape point shape =
    maybe 
    False 
    (\column -> 
        column
        & takeWhile (> y point)
        & length
        & odd)
    (Map.lookup (x point) shape)

pointInsideShape :: Point -> Map.Map Row [Column] -> Set.Set Point -> Bool
pointInsideShape p encodedShape borderPoints = 
    Set.member p borderPoints 
    || isPointInEncodedShape p encodedShape

part2 :: [Point] -> Maybe Rect
part2 input =
    rects
    & filter (all (\p -> pointInsideShape p shape shapeBorder) . cornerPointsInRect)
    & find (all (\p -> pointInsideShape p shape shapeBorder) . borderPointsInRect)
    where
        rects = sortedRects input
        shape = encodedShape input
        shapeBorder = shapePoints (lineSegmentsFromConsecutivePoints input)

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
