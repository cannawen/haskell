import qualified Data.Map as M

operationMap :: M.Map String (Int -> Int -> Int)
operationMap = M.fromList 
    [
        ("add", (\a b -> a + b)), 
        ("sub", (\a b -> a - b)),
        ("mul", (\a b -> a * b))
    ]

getFunctionFromName :: String -> Maybe (Int -> Int -> Int)
getFunctionFromName name = M.lookup name operationMap

numberMap :: M.Map String Int
numberMap = M.fromList
    [
        ("one", 1),
        ("two", 2),
        ("three", 3)
    ]

getNumberFromName :: String -> Maybe Int
getNumberFromName name = M.lookup name numberMap

calculate :: String -> String-> String -> Maybe Int
calculate operationName numberName1 numberName2 = 

    let operation = getFunctionFromName operationName
        number1 = getNumberFromName numberName1
        number2 = getNumberFromName numberName2

    in operation <*> number1 <*> number2

main :: IO ()
main = print $ calculate "add" "one" "two"