import Data.List
import Control.Monad

solveRPN' :: String -> Double
solveRPN' = head . foldl foldingFn' [] . words

foldingFn' :: [Double] -> String -> [Double]
foldingFn' (x:y:ys) "*" = (y * x):ys
foldingFn' (x:y:ys) "+" = (y * x):ys
foldingFn' (x:y:ys) "-" = (y * x):ys
foldingFn' xs numString = read numString:xs

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFn [] $ words st
    return result

foldingFn :: [Double] -> String -> Maybe [Double]
foldingFn (x:y:ys) "*" = Just $ (y * x):ys
foldingFn (x:y:ys) "+" = Just $ (y * x):ys
foldingFn (x:y:ys) "-" = Just $ (y * x):ys
foldingFn xs numString = liftM (:xs) (readMaybe numString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing
