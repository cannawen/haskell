parse input = input

part1 _ = "pt1"
part2 _ = "pt2"

--     https://adventofcode.com/2024/day/1
main :: IO ()
main = do
    contents <- readFile "app/aoc/2024/1/input.txt"
    let numbers = parse contents

    print (part1 numbers)
    print (part2 numbers)
