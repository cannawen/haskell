import Control.Exception (evaluate)
import Data.Function ((&))
import System.IO
import System.Environment

main = do
    (cmd:args) <- getArgs
    dispatch cmd args

dispatch :: String -> [String] -> IO ()
dispatch "add" = addTodo
dispatch "delete" = deleteTodo
dispatch _ = printTodo

deleteTodo [numberString] = do
    contents <- readFile "todo.txt"
    
    _ <- evaluate (length contents)
    
    let items = zip [0..] (lines contents)
    let numberedItems = filter (\(i, item) -> i /= read numberString) items & map snd
    
    writeFile "todo.txt" (unlines numberedItems)

printTodo [] = do
    withFile "todo.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let items = zip [0..] (lines contents)
        let numberedItems = map (\(i, item) -> show i ++ " - " ++ item) items
        mapM_ putStrLn numberedItems)


addTodo [todoItem] = do 
    withFile "todo.txt" AppendMode (\handle -> do
        hPutStrLn handle todoItem)

