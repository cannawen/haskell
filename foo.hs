main :: IO ()                                                                 
main = print [(a, b) | a <- [3, 2, 4], b <- [3, 2, 4], a < b, (a + b) == 6]