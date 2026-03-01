--     https://adventofcode.com/2025/day/1

parse :: String -> [Int]
parse = map f . lines 
  where f ('L':xs) = - read xs
        f ('R':xs) = read xs

part1 = length . filter (== 0) . scanl (\current num -> rem (current + num) 100) 50
part2 = sum . map fst . scanl f (50, 0)
  where f (zeros, current) num = ( abs ( fst ( divMod (current + num) 100)) , snd ( divMod (current + num) 100))

main :: IO ()
main = do
    contents <- readFile "app/aoc/2025/1/input.txt"
    let numbers = parse contents

    print (part1 numbers) 
    print (part2 numbers)
