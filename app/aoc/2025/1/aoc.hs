
-- parse contents = map (\(dir, num) -> if dir == 'R' then num else - num) parsedInstructions
--     where instructionStrings = lines contents
--           directions = map (\instruction -> instruction !! 0) instructionStrings
--           numbers = map read (map tail instructionStrings) :: [Int]
--           parsedInstructions = zip directions numbers

parse :: String -> [Int]
parse = map f . lines 
  where f ('L':xs) = - read xs
        f ('R':xs) = read xs

part1 = snd . foldl 
            (\(current, zeroCount) num -> (rem (current + num) 100, if rem (current + num) 100 == 0 then zeroCount + 1 else zeroCount)) 
            (50, 0) 
            

main :: IO ()
main = do
--     https://adventofcode.com/2025/day/1
    contents <- readFile "app/aoc/2025/1/input.txt"
    let numbers = parse contents

    print (part1 numbers)

