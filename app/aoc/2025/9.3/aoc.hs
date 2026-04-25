
import Data.Function ( (&) )
import Data.List.Split ( splitOn )
import Data.List ( foldl', find, sort, sortBy )
import Data.Ord ( comparing, Down(Down) )
import qualified Data.Map as Map
import qualified Data.Set as Set

-- TODO : nail down domain-specific terms and use consistently throughout the program.
-- Too many things referred to as "shape"

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Eq, Ord)

type LineSegment = (Point, Point)

isVertical :: LineSegment -> Bool
isVertical (p1, p2) = y p1 == y p2

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

intermediaryPointsFromSegment :: LineSegment -> [Point]
intermediaryPointsFromSegment (p1, p2) =
    if isVertical (p1, p2)
        then [Point x (y p1) | x <- [min (x p1) (x p2) .. max (x p1) (x p2) & pred]]
        -- This pred thing is kinda weird, but needed for ray tracing
        -- I don't love that the function name doesn't signify this
        -- and we are using the fn in a lot of places - it just happens to work out.
        -- TODO
        else [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]

lineSegmentsFromCornerPoints :: [Point] -> [LineSegment]
lineSegmentsFromCornerPoints points = zip points (rotate points)
    where rotate arr = tail arr ++ [head arr]

sortedRects :: [Point] -> [Rect]
sortedRects input =
    [(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (uncurry makeRect)
    & sortBy (comparing Down)

type Row = Int

type Column = Int

encodedShape :: [Point] -> Map.Map Row [Column]
encodedShape points = encode shapePointsVertical xBounds
    where
        xBounds = points & map x & maximum & succ
        shape = lineSegmentsFromCornerPoints points
        shapePointsVertical = Set.unions (map (Set.fromList . intermediaryPointsFromSegment) (filter isVertical shape))

encode :: Set.Set Point -> Int -> Map.Map Row [Column]
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

shapePoints :: [LineSegment] -> Set.Set Point
shapePoints shape = Set.unions (map (Set.fromList . intermediaryPointsFromSegment) shape)

isPointInEncodedShape :: Point -> Map.Map Row [Column] -> Bool
isPointInEncodedShape p s =
    case Map.lookup (x p) s of
    Nothing -> False
    Just columns -> odd $ length (takeWhile (> y p) columns)

pointInsideShape :: Point -> Map.Map Row [Column] -> Set.Set Point -> Bool
pointInsideShape p encodedShape borderPoints =
    Set.member p borderPoints || isPointInEncodedShape p encodedShape

part2 :: [Point] -> Maybe Rect
part2 input =
    filter (all (\p -> pointInsideShape p shape shapeBorder) . cornerPointsInRect) rect
    & find (all (\p -> pointInsideShape p shape shapeBorder) . borderPointsInRect)
    where
        rect = sortedRects input
        shape = encodedShape input
        shapeBorder = shapePoints (lineSegmentsFromCornerPoints input)

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
