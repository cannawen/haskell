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

combineCircuits a b = Circuit (Set.union (points a) (points b))

a = Point 1 2 3
b = Point 4 5 6
c = Point 7 8 9

d = Circuit (Set.fromList [a, b])
e = Circuit (Set.fromList [c])
f = Circuit (Set.fromList [a, c])

merge :: Set.Set Circuit -> Circuit -> Set.Set Circuit
merge existingCircuit newCircuit = Set.insert combinedAllCircuits independentCircuit
    where independentCircuit = Set.filter (\circuit ->  Set.disjoint (points circuit) (points newCircuit)) existingCircuit
          combinedExistingCircuits = foldl combineCircuits (Circuit Set.empty) (Set.difference existingCircuit independentCircuit) 
          combinedAllCircuits = combineCircuits combinedExistingCircuits newCircuit

-- part1 :: [Point]
part1 input = final & Set.toList & map points & map length & sort & reverse & take 3 & foldr1 (*)

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
            & take 1000
            & map (\(_, p1, p2) -> Circuit (Set.fromList [p1, p2]))

          final = foldl'
            (\memo points -> merge memo points)
            (Set.empty :: Set.Set Circuit)
            twoPointCircuits

main = do
    contents <- readFile "app/aoc/2025/8/input.txt"

    print $ part1 $ parse contents
