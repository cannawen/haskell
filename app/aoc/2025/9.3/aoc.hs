
import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Ord
import Data.Time
import Control.Monad
import qualified Data.Map as Map
import qualified Control.Applicative as Map

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

exampleInput = parse <$> readFile "app/aoc/2025/9/input-mini.txt"

type Row = Int
type Column = Int
-- type NumLinesToLeft = Int

type EncodedShape = Map.Map Row [Column]

-- encode :: Set.Set Point -> Int -> EncodedShape
-- encode shapeV xBound = Map.fromList [(xi, (map y (Set.toList (Set.filter (\p -> x p == xi) shapeV)))) |  xi <- [0 .. xBound]]

showCol :: [Column] -> String
showCol c = show c -- map (\col -> show col ++ ", " ) c & concat

showEncodedShape :: EncodedShape -> String
showEncodedShape shape = Map.toList shape
    & map (\(row, colList) -> show row ++ ": " ++ show colList ++ "\n")
    & concat

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

borderPointsInRect :: Rect -> [Point]
borderPointsInRect s =
    [Point (xMin s) (yMin s), Point (xMin s) (yMax s), Point (xMax s) (yMax s), Point (xMax s) (yMin s)]
    & shapeFromPoints
    & map pointsFromSegment
    & concat

-- cornerPointsInRect :: Rect -> ([Point], Rect)
-- cornerPointsInRect s =  (
--     [
--         Point (xMin s) (yMin s),
--         Point (xMin s) (yMax s),
--         Point (xMax s) (yMax s),
--         Point (xMax s) (yMin s)
--     ], s)

pointsFromSegment:: LineSegment -> [Point]
pointsFromSegment (p1, p2) =
    if isHorizontal (p1, p2)
        then [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]
        else [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]

shapeFromPoints :: [Point] -> [LineSegment]
shapeFromPoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

parse :: String -> [Point]
parse input =
    map (((\arr -> Point (head arr) (last arr)) . (map read)) . splitOn ",") (lines input)

-- Part 1 ----------------------------------------------------------------------------------------------------------------

part1 input =  
    [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (uncurry makeRect)
    & sort
    & reverse


-- shapePointsHorizontal shape = Set.unions (map (Set.fromList . pointsFromSegment) (filter isHorizontal shape))
-- shapePointsVertical shape = Set.unions (map (Set.fromList . pointsFromSegment) (filter (not . isHorizontal) shape))
shapePoints shape = Set.unions (map (Set.fromList . pointsFromSegment) shape)

-- Part 2 ----------------------------------------------------------------------------------------------------------------


pt2' shape input = pt2 shape (part1 input) (shapePoints $ shapeFromPoints input)


isPointInEncodedShape :: Point -> EncodedShape -> Bool
isPointInEncodedShape p s = 
    case cols of 
    Nothing -> False
    Just columns -> odd $ length (takeWhile (< y p) columns)
    where cols = Map.lookup (x p) s
    

pointInsideShape :: Point -> EncodedShape -> Set.Set Point -> Bool
pointInsideShape p encodedShape borderPoints = 
    Set.member p borderPoints || isPointInEncodedShape p encodedShape

pt2 :: EncodedShape -> [Rect] -> ShapePoints -> Maybe Rect
pt2 shape rect shapeBorder = 
    find 
    (\r -> 
        all 
        (\p -> pointInsideShape p shape shapeBorder) 
        (borderPointsInRect r)) 
    rect

main = do
    now <- getCurrentTime
    putStrLn (formatTime defaultTimeLocale "%A, %B %e, %Y - %H:%M:%S" now)

    contents <- readFile "app/aoc/2025/9/input-mini.txt"
    savedShape <- readFile "app/aoc/2025/9.3/output.txt"
    print $ pt2' (read savedShape :: EncodedShape) (parse contents)

    done <- getCurrentTime
    putStrLn (formatTime defaultTimeLocale "%A, %B %e, %Y - %H:%M:%S" done)
