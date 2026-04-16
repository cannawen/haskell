import System.IO
import Data.Char
import Data.List

main = moreFunctor

functor = do
    line <- fmap reverse getLine
    putStrLn $ "you said " ++ line ++ " backwards"

moreFunctor = do
    line <- fmap (intersperse '-' . map toUpper . reverse) getLine
    putStrLn $ "you said " ++ line ++ " backwards"

noFunctor = do
    line <- getLine
    putStrLn $ "you said " ++ reverse line ++ " backwards"

maybeReturner name = 
    case name of 
        Nothing -> Nothing 
        Just n -> Just (length n)

liftedMaybeReturner name = fmap length name

data User = User { userId :: Int, userName :: String }

getIds :: [User] -> [Int]
getIds users = [userId u | u <- users]

liftedGetIds users = fmap userId users
