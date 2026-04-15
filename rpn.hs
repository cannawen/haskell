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

calculate ::[Int] -> String ->  [Int]
calculate (a:b:ns) "+" = (b + a):ns
calculate (a:b:ns) "-" = (b - a):ns
calculate (a:b:ns) "*"  = (b * a):ns
calculate (a:b:ns) "/" = div b a:ns
calculate ns numberSting = read numberSting:ns

rpn :: String -> Int
rpn args = foldl calculate [] (words args) & head
