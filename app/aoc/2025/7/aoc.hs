import Data.Function ((&))
import Data.List.Split (splitOn, splitWhen)
import Data.List ( elemIndices, intercalate, sort )
import qualified Data.Text as T
import qualified Data.List.Split.Internals as T
import qualified Data.Map as Map
import Text.Show.Pretty (pPrint)
import Data.Maybe (fromMaybe)

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

parse :: String -> [[Int]]
parse input = map (map convertToInt) (lines input)
  & filter (\line -> sum line /= 0)
  where convertToInt c = case c of
                          'S' -> ray
                          '^' -> splitter
                          _ -> 0

-- collatedRowDeltas :: [(Int, Int)] -> [(Int, Int)]
collatedRowDeltas deltas =
  foldl
  (\memo (i, val) ->
    if fst (head memo) == i
      then (i, snd (head memo) + val ) : tail memo
    else
      (i, val) : memo)
  [head deltas]
  (tail deltas)
  & Map.fromList

rowDeltas prevRow currRow =
          foldl
          (\memo (i, (prevRowVal, currRowVal)) ->
            if currRowVal == splitter && prevRowVal > 0
              then (i - 1, prevRowVal) : (i + 1, prevRowVal) : memo
            else if currRowVal == 0 && prevRowVal > 0
              then (i, prevRowVal) : memo
            else
              memo)
          []
          (zip [0..] (zip prevRow currRow)) & sort

modifyCurrentRow' :: [Int] -> [Int] -> [Int]
modifyCurrentRow' prevRow currRow =
  map (\(i, value) ->
    fromMaybe
      value
      (Map.lookup i (collatedRowDeltas $ rowDeltas prevRow currRow)))
  (zip [0..] currRow)


part2 input = foldl1 modifyCurrentRow' input & filter (>0) & sum
-- part2 input = scanl1 modifyCurrentRow' input

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
  parse contents & part2 & print
  -- parse contents & part2 & prettyMatrix & putStrLn
