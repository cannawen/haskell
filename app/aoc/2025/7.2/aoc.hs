import Data.Function ((&))
import qualified Data.Vector as V
import Data.List (sort)

data Tile
    = Splitter
    | Path Int
    deriving (Show, Eq, Ord)

combineTiles :: Int -> Tile -> Tile -> [(Int, Tile)]
combineTiles i (Path prev) (Path curr) = [(i, Path (prev + curr))]
combineTiles i (Path prev) (Splitter) = [(i-1, Path (prev)), (i+1, Path (prev)), (i, Splitter)]
combineTiles i prev curr = [(i, curr)]

-- newtype Row  = Row [Tile] deriving (Show, Eq)
-- newtype Grid = Grid [Row] deriving (Show, Eq)
type Row  = V.Vector Tile
type Grid = V.Vector Row

parse :: String -> Grid
parse contents = lines contents
    & map (map (\c ->
        if c == 'S' then Path 1 else
        if c == '^' then Splitter else
        Path 0 ))
    & filter (any (/= Path 0))
    & map V.fromList
    & V.fromList

calculateNewRow :: Row -> Row -> Row
calculateNewRow prevRow currRow = currRow
    where 
        deltas = 
            map 
            (\(i, prevRowTile, currRowTile) -> 
                -- this transformation turns splitters into Path 0
                if prevRowTile > Path 0 && currRowTile == Splitter then [(i-1, prevRowTile), (i, Path 0), (i+1, prevRowTile)] else [(i, prevRowTile)] ) 
            (zip3 [0..] (V.toList prevRow) (V.toList currRow))
            & concatMap sort
        collate = 
            foldl 
            (\memo newDelta -> if fst (head memo) == fst newDelta then (fst (head memo), snd (head memo) + snd newDelta):memo else newDelta:memo) 
            [head deltas]
            (tail deltas)
        

        
        

part1 :: Grid -> Int
part1 grid = 2
    where x = foldl1 calculateNewRow grid

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/7/input-mini.txt"
    print (parse contents)
