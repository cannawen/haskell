module Euler where

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do 
    putStrLn "https://projecteuler.net/problem=1"
    print (sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0])

    putStrLn "https://projecteuler.net/problem=2"
    print (sum [fib x | x <- [1..], fib x < 4000000, fib x `mod` 2 == 0])
