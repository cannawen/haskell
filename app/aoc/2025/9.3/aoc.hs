
import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Ord
import qualified Data.Map as Map

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord)

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

cornerPointsInRect :: Rect -> [Point]
cornerPointsInRect s =
    [
        Point (xMin s) (yMin s),
        Point (xMin s) (yMax s),
        Point (xMax s) (yMax s),
        Point (xMax s) (yMin s)
    ]

borderPointsInRect :: Rect -> [Point]
borderPointsInRect s =
    [
        Point (xMin s) (yMin s), 
        Point (xMin s) (yMax s), 
        Point (xMax s) (yMax s), 
        Point (xMax s) (yMin s)
    ]
    & lineSegmentsFromCornerPoints
    & concatMap intermediaryPointsFromSegment

type Row = Int
type Column = Int
type EncodedShape = Map.Map Row [Column]

encode :: Set.Set Point -> Int -> EncodedShape
encode shapeV xBound = 
    Set.toList shapeV 
    & sort 
    & foldl' 
      (\m p -> 
          if fst (head m) == x p 
          then (x p, y p : snd (head m)) : tail m 
          else (x p, [y p]) : m) 
      [(0,[])]
    & Map.fromList

intermediaryPointsFromSegment:: LineSegment -> [Point]
intermediaryPointsFromSegment (p1, p2) =
    if isHorizontal (p1, p2)
        then [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]
        else [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]

lineSegmentsFromCornerPoints :: [Point] -> [LineSegment]
lineSegmentsFromCornerPoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

parse :: String -> [Point]
parse input =
    map (((\arr -> Point (head arr) (last arr)) . (map read)) . splitOn ",") (lines input)

-- Part 1 ----------------------------------------------------------------------------------------------------------------

part1 input =
    [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (uncurry makeRect)
    & sortBy (comparing Down)

shapePoints shape = Set.unions (map (Set.fromList . intermediaryPointsFromSegment) shape)

-- Part 2 (code to save encoded shape) ----------------------------------------------------------------------------------------------------------------

part2Saving points = encode shapePointsVertical xBounds
    where
        xBounds = points & map x & maximum & succ
        shape = lineSegmentsFromCornerPoints points
        shapePointsVertical = Set.unions (map (Set.fromList . intermediaryPointsFromSegment) (filter (not . isHorizontal) shape))

-- Part 2 ----------------------------------------------------------------------------------------------------------------

part2 shape input =
    filter (all (\p -> pointInsideShape p shape shapeBorder) . cornerPointsInRect) rect
    & find (all (\p -> pointInsideShape p shape shapeBorder) . borderPointsInRect)
    where
        rect = part1 input
        shapeBorder = shapePoints (lineSegmentsFromCornerPoints input)

isPointInEncodedShape :: Point -> EncodedShape -> Bool
isPointInEncodedShape p s =
    case Map.lookup (x p) s of
    Nothing -> False
    Just columns -> odd $ length (takeWhile (> y p) columns)

pointInsideShape :: Point -> EncodedShape -> Set.Set Point -> Bool
pointInsideShape p encodedShape borderPoints =
    Set.member p borderPoints || isPointInEncodedShape p encodedShape

main = do
    contents <- readFile "app/aoc/2025/9/input.txt"
    -- writeFile "app/aoc/2025/9.3/output.txt" (show $ part2Saving $ parse contents)
    savedShape <- readFile "app/aoc/2025/9.3/output.txt"
    print $ size <$> part2 (read savedShape :: EncodedShape) (parse contents)
