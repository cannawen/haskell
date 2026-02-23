main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/1/input.txt"
    putStrLn contents