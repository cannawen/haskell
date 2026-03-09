import Data.Function ((&))
import Data.List.Split (splitOn, splitWhen)
import Data.List (elemIndices)
import qualified Data.Text as T
import qualified Data.List.Split.Internals as T
import qualified Data.Map as Map
import Text.Show.Pretty (pPrint)
import Data.List (intercalate)

-- Equivalent to Clojure's frequencies
frequencies :: Ord a => [a] -> Map.Map a Int
frequencies xs = Map.fromListWith (+) [(x, 1) | x <- xs]

modifyCurrentRow prevRow row = 
  let findHit = map (\(prev, curr) -> if prev == 'S' && curr == '.' then 'S' else 
                                     if prev == 'S' && curr == '^' then 'h' else 
                                     curr) (zip prevRow row)
      hIndices = elemIndices 'h' findHit
      newSIndices = concatMap (\i -> [i-1, i+1]) hIndices
      newRow = map (\(i, curr) -> if elem i newSIndices then 'S' else curr) (zip [0..] findHit)

  in newRow

part1 input = scanl1 modifyCurrentRow input & concat & filter (=='h') & length

------

ray = 1
hitSplitter = -1
splitter = -2
newline = -3

parse :: String -> [[Int]]
parse input = splitOn [newline] (map (\c -> case c of 
                                      '.' -> 0
                                      'S' -> ray
                                      '^' -> splitter
                                      _ -> newline) input)

modifyCurrentRow' prevRow row = 
  let findHit = map (\(prev, curr) -> if prev > 0 && curr == 0 then prev else 
                                      if prev > 0 && curr == splitter then hitSplitter else 
                                      curr) (zip prevRow row)
      hitSplitterIndices = elemIndices hitSplitter findHit
      newRayIndices = Map.unionsWith (+) (map (\i -> Map.fromList [(i-1,prevRow!!i),(i+1,prevRow!!i)]) hitSplitterIndices)
      newRow = map (\(i, curr) -> 
        if curr < 0 then
          curr
        else
          Map.findWithDefault 0 i newRayIndices 
          + if prevRow!!i > 0 then prevRow!!i else 0
          ) (zip [0..] findHit)

  in newRow

part2 input = foldl1 modifyCurrentRow' input & sum


prettyMatrix :: [[Int]] -> String
prettyMatrix rows =
  let width = maximum (map (length . show) (concat rows))
      pad n =
        let s = show n
        in replicate (width - length s) ' ' ++ s
      renderRow r = "[ " ++ intercalate " " (map pad r) ++ " ]"
  in unlines (map renderRow rows)

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
  contents <- readFile "app/aoc/2025/7/input.txt"

  lines contents & part1 & print
  parse contents & part2 & print -- prettyMatrix & putStrLn
