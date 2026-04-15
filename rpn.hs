import Data.Function ((&))
import System.IO
import System.Environment
import Text.Read
import Data.Maybe

main = do
    args <- getArgs
    if null args
        then print (rpn "10 4 3 + 2 * -")
        else print (rpn (head args))

isInt :: String -> Bool
isInt s = isJust (readMaybe s :: Maybe Int)

calculate :: String -> Int -> Int -> Int
calculate "+" a b = a + b
calculate "-" a b = a - b
calculate "*" a b = a * b
calculate "/" a b = div a b
calculate _ a b = 0

rpn :: String -> Int
rpn args = foldl (\memo token -> 
    if isInt token 
        then read token : memo 
        else calculate token (memo !! 1) (head memo) : (drop 2 memo) ) [] (words args) !! 0