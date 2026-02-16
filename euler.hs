module Euler where

main = do 
    putStrLn "https://projecteuler.net/problem=1"
    print (sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0])
