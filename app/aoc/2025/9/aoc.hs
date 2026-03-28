import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
    { x :: Double
    , y :: Double
    } deriving (Show, Eq, Ord)

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (last arr))

part1 input = input

part2 input = input

main = do
    contents <- readFile "app/aoc/2025/9/input.txt"

    print $ part1 $ parse contents
    -- print $ part2 $ parse contents
