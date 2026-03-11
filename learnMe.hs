-- ghci learnMe.hs
-- `ghci> :r` to reload

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

numbers = [1, 2, 3, 4] ++ [5, 6, 7, 8]
words = "hello" ++ " " ++ ['w', 'o', 'r', 'l', 'd']

-- More efficient; adding at the front
prependNumbers = 5 : [4, 3, 2, 1]
prependNumbers' = 5:4:3:2:1:[]
prependWords = 'a' : " lot of words"

indexOut = prependNumbers !! 1

-- [1, 2, 3] < [2, 3, 4] -- True
-- [1, 2, 3] < [1, 2, 2] -- False
-- [1, 2, 3] < [1, 3, 2] -- True
-- [1, 2] < [1, 2, 3] -- True
-- [11] < [1, 2, 3] -- False

-- Learn how to write recursion
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

max' [] = Nothing
max' [x] = Just x
max' (x:xs) = max (Just x) (max' xs)

replicate' :: Int -> a -> [a]
replicate' 1 y = [y]
replicate' n y = y : replicate' (n-1) y

replicate'' n y
    | n <= 0 = []
    | otherwise = y : replicate'' (n-1) y
