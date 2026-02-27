module Euler (main, one, two, three, seven, fib, isPrime, primes, primeFactors) where

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

one = do 
    putStrLn "https://projecteuler.net/problem=1"
    print (sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0])

two = do
    putStrLn "https://projecteuler.net/problem=2"
    print (sum [fib x | x <- [1..], fib x < 4000000, fib x `mod` 2 == 0])


isPrime_simple n = hasDivisorsMoreThan 2
    where
        hasDivisorsMoreThan d
            | d*d > n        = True
            | n `rem` d == 0 = False
            | otherwise      = hasDivisorsMoreThan (d+1)

isPrime n = null [x | x <- [2..(ceiling.sqrt.fromIntegral) n], n `rem` x == 0]

primes = filter isPrime [2 .. ]

primeFactors n = filter (\p -> n `rem` p == 0) (takeWhile (\p -> p * p < n) primes)


three = do
    putStrLn "https://projecteuler.net/problem=3"
    print (primeFactors 600851475143)

seven = do
    putStrLn "https://projecteuler.net/problem=7"
    print (primes !! 10000)


main = three
