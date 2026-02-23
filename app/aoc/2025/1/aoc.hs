main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/1/input.txt"
    let instructions = lines contents
    let directions = map (\instruction -> instruction !! 0) instructions
    let numbers = map tail instructions
    print (take 5 directions)
    print (take 5 numbers)
