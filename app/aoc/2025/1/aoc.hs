main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/1/input.txt"
    let inputLines = lines contents
    print (take 5 inputLines)
