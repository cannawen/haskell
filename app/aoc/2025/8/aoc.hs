import Data.Function ((&))
import Data.List.Split
import Data.List
import qualified Data.Set as Set

parse :: String -> [(Int, Int, Int)]
parse input = 
    lines input
    & map (splitOn ",") 
    & map (\point -> map read point)
    & map (\arr -> (head arr, arr !! 1, arr !! 2))

mergePointsIntoSets :: Set.Set (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
mergePointsIntoSets setSet p1 p2 = 
    if setsToMerge == []
    then Set.insert (Set.fromList [p1, p2]) setSet
    else setSet
    where setsToMerge = Set.filter (\set -> Set.member p1 set || Set.member p2 set) setSet 

part1 input =  final & map Set.size & sort & reverse & take 3
    where combinations = [(p1, p2) | (p1:rest) <- tails input, p2 <-rest] & sort
          distancesSquared = 
            map 
                (\(p1@(x1, y1, z1), p2@(x2, y2, z2)) -> 
                    ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2, p1, p2))
                combinations
            & sort
            & take 10
          final = foldl 
            (\memo (_, p1, p2)  -> mergePointsIntoSets memo p1 p2)
            Set.empty 
            distancesSquared
            
main = do
    contents <- readFile "app/aoc/2025/8/input-mini.txt"

    print $ part1 $ parse contents
