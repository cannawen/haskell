import System.IO
import Data.Char
import Data.List

main = upFunctor

functor = do
    line <- fmap reverse getLine
    putStrLn $ "you said " ++ line ++ " backwards"

upFunctor = do
    line <- fmap (intersperse '-' . map toUpper . reverse) getLine
    putStrLn $ "you said " ++ line ++ " backwards"

noFunctor = do
    line <- getLine
    putStrLn $ "you said " ++ reverse line ++ " backwards"
