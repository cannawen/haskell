import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFn [] . words

foldingFn :: [Double] -> String -> [Double]
foldingFn (x:y:ys) "*" = (y * x):ys
foldingFn (x:y:ys) "+" = (y * x):ys
foldingFn (x:y:ys) "-" = (y * x):ys
foldingFn xs numString = read numString:xs