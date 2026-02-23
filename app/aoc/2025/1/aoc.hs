main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/1/input-mini.txt"
    let instructionStrings = lines contents

    let directions = map (\instruction -> instruction !! 0) instructionStrings
    let numbers = map read (map tail instructionStrings) :: [Int]

    let parsedInstructions = zip directions numbers
    let numbers = map (\(dir, num) -> if dir == 'R' then num else - num) parsedInstructions

    let answer = foldl 
            (\(current, zeroCount) num -> (rem 100 (current + num), if rem 100 (current + num) == 0 then zeroCount + 1 else zeroCount)) 
            (0, 0) 
            numbers

    print (snd answer + 1)

