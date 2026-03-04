import Data.List.Split (splitOn)

parse input = map (splitOn "-") (splitOn "," input)

part1 = length . filter (== 0) . scanl (\current num -> rem (current + num) 100) 50

part2 numbers = part1 (expandOnes numbers)
  where expandOnes = concatMap (\n -> replicate (abs n) (signum n))

--     https://adventofcode.com/2025/day/2
main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/2/input-mini.txt"
    let numbers = parse contents
    print numbers

    -- print (part1 numbers)
    -- print (part2 numbers)
