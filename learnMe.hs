doubleMe x = x + x  
doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2

numbers = [1, 2, 3, 4] ++ [5, 6, 7, 8]
words = "hello" ++ " " ++ ['w', 'o', 'r', 'l', 'd']

-- More efficient; adding at the front
prependNumbers = 5: [4, 3, 2, 1]
prependNumbers' = 5:4:3:2:1:[]
prependWords = 'a': " lot of words"

indexOut = prependNumbers !! 1

-- [1, 2, 3] < [2, 3, 4] -- True
-- [1, 2, 3] < [1, 2, 2] -- False
-- [1, 2, 3] < [1, 3, 2] -- True
-- [1, 2] < [1, 2, 3] -- True
-- [11] < [1, 2, 3] -- False
