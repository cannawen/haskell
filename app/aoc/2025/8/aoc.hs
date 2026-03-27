import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

-- type Point = (Int, Int, Int)
data Point = Point
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving (Show, Eq, Ord)

newtype Circuit = Circuit 
    { points :: Set.Set Point }
    deriving (Show, Eq, Ord)

parse :: String -> [Point]
parse input =
    lines input
    & map (splitOn ",")
    & map (\point -> map read point)
    & map (\arr -> Point (head arr) (arr !! 1) (arr !! 2))

distance p1 p2 = (x p1 - x p2)^2 + (y p1 - y p2)^2 + (z p1 - z p2)^2

merge existingCircuit newCircuit = existingCircuit

-- part1 :: [Point]
part1 input = input
    where combinations =
            [Set.fromList [p1, p2] | p1 <- input, p2 <- input, p1 /= p2]
            & Set.fromList
            & Set.map Set.toList
            & Set.toList
            & sort

          twoPointCircuits =
            map
                (\[p1, p2] -> (distance p1 p2, p1, p2))
                combinations
            & sort
            & take 10
            & map (\(_, p1, p2) -> Circuit (Set.fromList [p1, p2]))

          final = foldl'
            (\memo points -> merge memo points)
            (Set.empty :: Set.Set Circuit)
            twoPointCircuits

main = do
    contents <- readFile "app/aoc/2025/8/input-mini.txt"

    print $ part1 $ parse contents
