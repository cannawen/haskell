import Data.Function ((&))
import Data.List (sort, intercalate)

data Tile
    = Splitter
    | Path Int
    deriving (Eq, Ord)

instance Show Tile where
    show Splitter = "^"
    show (Path 0) = "."
    show (Path n) = show n

combineTiles :: (Int, Tile, Tile) -> [(Int, Tile)]
combineTiles (i, Path prev, Path curr) = [(i, Path (prev + curr))]
combineTiles (i, Path prev, Splitter) = [(i-1, Path prev), (i+1, Path prev), (i, Splitter)]
combineTiles (i, prev, curr) = [(i, curr)]

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
        deltas = concatMap sort $ map combineTiles (zip3 [0..] prevRow currRow)
        collated =
            foldl
            (\memo newDelta ->
                let (prevIndex, prevTile) = head memo
                    (currIndex, currTile) = newDelta
                in if prevIndex == currIndex then combineTiles (prevIndex, prevTile, currTile) ++ tail memo else newDelta : memo)
            [head deltas]
            (tail deltas)
            & sort

part2 :: Grid -> Int
part2 grid = sumRow (foldl1 calculateNewRow grid)

-- part2 grid = scanl1 calculateNewRow grid

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/7/input-mini.txt"
    print $ part2 $ parse contents
