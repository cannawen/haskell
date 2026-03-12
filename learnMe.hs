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

take' :: Int -> [a] -> [a]
take' n (x:xs)
    | n <= 0 = []
    | n > length xs = x:xs
    | otherwise = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = if e == x then True else elem' e xs

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = (quicksort' $ filter (<x) xs) ++ [x] ++ (quicksort' $ filter (>=x) xs)

quicksort'' :: Ord a => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort'' [y | y <- xs, y < x] ++ [x] ++ quicksort'' [y | y <- xs, y >= x]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> (a, b)) -> [a] -> [b] -> [(a, b)]
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = (x,y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g = (\ x y -> f y x)
    -- where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = (\ x y -> f y x)

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f x y = f y x

max'' :: Ord a => a -> a -> a
max'' x y = if x > y then x else y

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) 
    | f x = x : filter' f xs
    | otherwise = filter' f xs

x = head $ reverse' $ filter' (\x -> mod x 3829 == 0) [1..100000]
y = sum $ takeWhile (<10000) $ filter odd [x*x | x <-[1..]] 

collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq i 
    | mod i 2 == 0 = i : collatzSeq (div i 2) 
    | otherwise = i : collatzSeq (i * 3 + 1)

map'' :: (a -> b -> c) -> [a] -> [b] -> [c]
map'' _ _ [] = []
map'' _ [] _ = []
map'' f (a:as) (b:bs) = f a b : map'' f as bs

mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

elemr :: (Eq a) => a -> [a] -> Bool
elemr x xs = foldr (\elem acc -> acc || elem == x) False xs

add x y = x+y
zr = foldr add 0 [3, 2, 1]
z = add 3 (add 2 (add 1 0))

zl = foldl add 0 [3, 2, 1]

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

z' = sum (filter ( > 10) (map (*2) [2..10]))
z'' = sum $ filter ( > 10) $ map (*2) [2..10]

a :: [Double]
a = map ($ 3.0) [(4 +), (10 *), (^ 2), sqrt]

b = map (negate . abs) [5, -3, 6, 7]
c = map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
c' = map (negate. sum . tail) [[1..5], [3..6], [1..7]]

d = sum (replicate 5 (max 6.7 8.9))
d' = (sum . replicate 5) (max 6.7 8.9)
d'' = sum . replicate 5 $ max 6.7 8.9

e = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
e' = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

f x = ceiling (negate (tan (cos (max 50 x))))
f' x = (ceiling . negate . tan . cos) $ max 50 x
f'' = ceiling . negate . tan . cos . max 50

