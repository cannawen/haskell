import Data.Function ((&))
import Data.List (sort, intercalate)

data Tile
    = Splitter
    | Path Int
    deriving (Show, Eq, Ord)

combineTiles :: Int -> Tile -> Tile -> [(Int, Tile)]
combineTiles i (Path prev) (Path curr) = [(i, Path (prev + curr))]
combineTiles i (Path prev) Splitter = [(i-1, Path prev), (i+1, Path prev), (i, Splitter)]
combineTiles i prev curr = [(i, curr)]

type Row  = [Tile]
type Grid = [Row]

parseTile :: Char -> Tile
parseTile 'S' = Path 1
parseTile '^' = Splitter
parseTile _ = Path 0

parse :: String -> Grid
parse contents = lines contents
    & map (map parseTile)
    & filter (any (/= Path 0))

sumRow :: Row -> Int
sumRow row = sum [n | Path n <- row]

calculateNewRow :: Row -> Row -> Row
calculateNewRow prevRow currRow = map snd collated
    where
        deltas =
            map
            (\(i, prevRowTile, currRowTile) ->
                combineTiles i prevRowTile currRowTile)
            (zip3 [0..] prevRow currRow)
            & concatMap sort
        collated =
            foldl
            (\memo newDelta -> 
                let (prevIndex, prevTile) = head memo
                    (currIndex, currTile) = newDelta
                in if prevIndex == currIndex then (combineTiles prevIndex prevTile currTile) ++ (tail memo) else newDelta:memo)
            [head deltas]
            (tail deltas)
            & sort

part2 :: Grid -> Int
part2 grid = sumRow (foldl1 calculateNewRow grid)

-- padLeft :: Int -> String -> String
-- padLeft w s = replicate (max 0 (w - length s)) ' ' ++ s

-- prettyGrid :: Grid -> String
-- prettyGrid grid =
--     let
--         maxVal = maximum (0 : [n | row <- grid, Path n <- row])
--         cellW = max 1 (length (show maxVal))
--         fmtTile t = case t of
--             Splitter -> padLeft cellW "^"
--             Path 0 -> padLeft cellW "."
--             Path n -> padLeft cellW (show n)
--         maxCols = maximum (0 : map length grid)
--         header =
--             "r\\c | " ++ intercalate " " [padLeft cellW (show c) | c <- [0 .. maxCols - 1]]
--         divider = replicate (length header) '-'
--         fmtRow (r, row) =
--             padLeft 3 (show r) ++ " | " ++ intercalate " " (map fmtTile row)
--     in
--         unlines (header : divider : map fmtRow (zip [0..] grid))

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/7/input-mini.txt"
    -- putStrLn (prettyGrid (scanl1 calculateNewRow (parse contents)))
    print $ part2 $ parse contents
