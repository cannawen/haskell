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

parse :: String -> [Point]
parse input =
    map ((\arr -> Point (head arr) (last arr)) . (\point -> map read point)) (lines input
    & map (splitOn ","))

-- Part 1 ----------------------------------------------------------------------------------------------------------------

squareSize p1 p2 =  succ (p1 - p2 & x & abs) * succ ((p1 - p2) & y & abs)

part1 input = maximum ([(p1, p2) | p1 <- input, p2 <- input, p1 < p2]
    & map (\(p1, p2) -> squareSize p1 p2))

-- Part 2 (Brute force) ----------------------------------------------------------------------------------------------------------------

rotate arr = tail arr ++ [head arr]

shouldSwitch input set x y = Set.member (Point x y) set -- && (not $ elem (Point x y) input)

part2_brute input = shapeH
    where outlineSet =
            concatMap (\(p1, p2) -> [Point x y | x <- [min (x p1) (x p2) .. max (x p1) (x p2)], y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]) (zip input (rotate input))
            & Set.fromList
          horizontalSet =
            concatMap (\(p1, p2) -> [Point (x p1) y | y <- [min (y p1) (y p2) .. max (y p1) (y p2)]]) (zip input (rotate input)
            & filter (\(p1, p2) -> x p1 == x p2))
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

data Square = Square
    { xMin :: Int
    , yMin :: Int
    , xMax :: Int
    , yMax :: Int
    } deriving (Eq, Show)

instance Ord Square where
    compare = comparing size

size s = (xMax s - xMin s) * (yMax s - yMin s)

makeSquare :: Point -> Point -> Square
makeSquare p1 p2 =
    Square {
        xMin = min (x p1) (x p2),
        yMin = min (y p1) (y p2),
        xMax = max (x p1) (x p2),
        yMax = max (y p1) (y p2)
    }

insetSquare :: Square -> Square
insetSquare s = Square {
    xMin = succ (xMin s),
    yMin = succ (yMin s),
    xMax = pred (xMax s),
    yMax = pred (yMax s)
}

allPointsBordering :: Square -> Set.Set Point
allPointsBordering s =
    [Point (xMin s) y | y <- [(yMin s) .. (yMax s)]]
    ++ [Point (xMax s) y | y <- [(yMin s) .. (yMax s)]]
    ++ [Point x (yMin s) | x <- [(xMin s) .. (xMax s)]]
    ++ [Point x (yMax s) | x <- [(xMin s) .. (xMax s)]]
    & Set.fromList

middleSquare :: Square -> Point
middleSquare s = Point
    ((xMin s + xMax s) `div` 2)
    ((yMin s + yMax s) `div` 2)

allBorderPoints :: [LineSegment] -> Set.Set Point
allBorderPoints s = map (allPointsBordering . uncurry makeSquare) s & Set.unions

lineToRight :: Point -> Int -> LineSegment
lineToRight point xMax = (Point (x point) (y point), Point xMax (y point))

-- assume no borders of only 1 distance between them, so any time you cross a border inside the target rectangle it's bad
isSquareInside :: Square -> [LineSegment] -> Int -> Bool
isSquareInside square shape xMaxBounds =
    Set.disjoint (allPointsBordering (insetSquare square)) (allBorderPoints shape) &&
    odd (numLinesCrossed (lineToRight (middleSquare square) xMaxBounds) shape)

-- isPointOnBounds :: Point -> [LineSegment] -> Bool
-- isPointOnBounds p bounds = foldl' (\m (l1, l2) -> m
--     || (x l1 == x l2 && x p == x l1 && y p <= max (y l1) (y l2) && y p >= min (y l1) (y l2))
--     || (y l1 == y l2 && y p == y l1 && x p <= max (x l1) (x l2) && x p >= min (x l1) (x l2))
--     ) False bounds

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
        min1 <= max2 && max2 <= max1
        || min1 <= min2 && min2 <= max1
        || min2 <= min1 && max2 >= max1
        || min2 >= min1 && max2 <= max1

numLinesCrossed :: LineSegment -> [LineSegment] -> Int
numLinesCrossed line shape = map ((\b -> if b then 1 else 0) . intersection line) shape & sum

-- isOnBorder :: Point -> [LineSegment] -> Bool
-- isOnBorder p shape = 
--     any (\(p1, p2) -> min (y p1) (y p2) <= y p && y p <= max (y p1) (y p2)) vertical
--     || any (\(p1, p2) -> min (x p1) (x p2) <= x p && x p <= max (x p1) (x p2)) horizontal
--     where
--         vertical = filter (\(p1, p2) -> x p1 == x p2 && x p == x p1) shape
--         horizontal = filter (\(p1, p2) -> y p1 == y p2 && y p == y p1) shape

-- isInShape :: Point -> Int -> [LineSegment] -> Bool
-- isInShape point xMax shape = isOnBorder point shape || odd (numLinesCrossed (lineToRight point xMax)  shape)

shapeCoords :: [Point] -> [LineSegment]
shapeCoords input = zip input (rotate input)

part2 input =
    [makeSquare p1 p2 | p1 <- input, p2 <- input, p1 < p2]
    & sortBy (comparing Down)
    & find (\square -> isSquareInside square (shapeCoords input) inputMaxX)
    where
        inputMaxX = map x input & maximum

main = do
    contents <- readFile "app/aoc/2025/9/input.txt"

    -- print $ part1 $ parse contents
    print $ part2 $ parse contents

exampleInput = parse <$> readFile "app/aoc/2025/9/input-mini.txt"
