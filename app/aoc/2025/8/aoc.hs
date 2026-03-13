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

part1 input = input
    where combinations = [(p1, p2) | (p1:rest) <- tails input, p2 <-rest] & sort
          distancesSquared = 
            map 
                (\(p1@(x1, y1, z1), p2@(x2, y2, z2)) -> 
                    ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2, p1, p2))
                combinations
            & sort
            & take 10
          final = foldl 
            (\memo (_, p1, p2)  -> 
                let connectp1p2ToExistingSets = map (\existingSet -> 
                        if (Set.member p1 existingSet)
                        then Set.insert p2 existingSet
                        else if (Set.member p2 existingSet)
                        then Set.insert p1 existingSet
                        else existingSet) memo
                in if filter (\x -> True) connectp1p2ToExistingSets)
            [] 
            distancesSquared
main = do
    contents <- readFile "app/aoc/2025/8/input-mini.txt"

    print $ part1 $ parse contents
