{-# LANGUAGE PatternSynonyms #-}
import qualified Data.Map as M
type Result e a = Either e a
pattern Fail e = Left e
pattern Success a = Right a







--------------------- ignore above; not important -------------------------












operationMap :: M.Map String (Int -> Int -> Int)
operationMap = M.fromList 
    [
        ("add", (\a b -> a + b)), 
        ("sub", (\a b -> a - b)),
        ("mul", (\a b -> a * b))
    ]

getFunctionFromName :: String -> Result String (Int -> Int -> Int)
getFunctionFromName name = maybe (Fail ("operation " ++ name ++ " not found")) Success (M.lookup name operationMap)

numberMap :: M.Map String Int
numberMap = M.fromList
    [
        ("one", 1),
        ("two", 2),
        ("three", 3)
    ]

getNumberFromName :: String -> Result String Int
getNumberFromName name = maybe (Fail ("number " ++ name ++ " not found")) Success (M.lookup name numberMap)

calculate :: String -> String-> String -> Result String Int
calculate operationName numberName1 numberName2 = 

    let operation = getFunctionFromName operationName
        number1 = getNumberFromName numberName1
        number2 = getNumberFromName numberName2

    in operation <*> number1 <*> number2

main :: IO ()
main = print $ calculate "add" "one" "two"
