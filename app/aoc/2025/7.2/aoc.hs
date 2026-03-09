import Data.Function ((&))

data Tile
    = Splitter
    | Path Int
    deriving (Show, Eq)

newtype Row  = Row [Tile] deriving (Show, Eq)
newtype Grid = Grid [Row] deriving (Show, Eq)

parse :: String -> Grid
parse contents = lines contents
    & map (map (\c ->
        if c == 'S' then Path 1 else
        if c == '^' then Splitter else
        Path 0 ))
    & filter (any (/= Path 0))
    & map Row
    & Grid

--     https://adventofcode.com/2025/day/7
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/7/input-mini.txt"
    print (parse contents)
