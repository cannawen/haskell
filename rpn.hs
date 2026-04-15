import Data.Function ((&))
import System.IO
import System.Environment

main = do
    args <- getArgs
    if null args
        then print (rpn "10 4 3 + 2 * -")
        else print (rpn (head args))

rpn :: String -> Int
rpn args = 0
