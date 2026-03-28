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

merge :: Set.Set Circuit -> Circuit -> Set.Set Circuit
merge existingCircuit newCircuit = Set.insert combinedAllCircuits independentCircuit
    where independentCircuit = Set.filter (\circuit ->  Set.disjoint (points circuit) (points newCircuit)) existingCircuit
          combinedExistingCircuits = foldl combineCircuits (Circuit Set.empty) (Set.difference existingCircuit independentCircuit) 
          combinedAllCircuits = combineCircuits combinedExistingCircuits newCircuit

part1 input = mergedCircuits & Set.toList & map points & map length & sort & reverse & take 3 & foldr1 (*)

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

          mergedCircuits = foldl' merge Set.empty twoPointCircuits

notIn smallCircuit bigCircuit = not (Set.isSubsetOf (points smallCircuit) (points bigCircuit))

oneInEach smallCircuit bigCircuitA bigCircuitB = 
        (Set.member p1 (points bigCircuitA) && Set.member p2 (points bigCircuitB)) 
        || (Set.member p2 (points bigCircuitA) && Set.member p1 (points bigCircuitB))
    where p = points smallCircuit & Set.toList
          p1 = head p
          p2 = last p

part2 input = answer & head 

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
            & map (\(_, p1, p2) -> Circuit (Set.fromList [p1, p2]))

          setOfAllSingleCircuits = map (\p -> (Circuit (Set.singleton p))) input & Set.fromList

          twoSeparateCircuits = scanl' merge setOfAllSingleCircuits twoPointCircuits
            -- & group & map head
            & filter (\circuit -> Set.size circuit == 2)
            & last
        
          bigCircuitA = Set.toList twoSeparateCircuits & head
          bigCircuitB = Set.toList twoSeparateCircuits & last

        --   pointsNotInEither = filter (\twoPoint -> notIn twoPoint bigCircuitA && notIn twoPoint bigCircuitB) twoPointCircuits

          answer = filter (\twoPoint -> oneInEach twoPoint bigCircuitA bigCircuitB) twoPointCircuits

main = do
    contents <- readFile "app/aoc/2025/8/input.txt"

    print $ part1 $ parse contents
    print $ part2 $ parse contents
