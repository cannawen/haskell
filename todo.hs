import Control.Exception (evaluate)
import Data.Function ((&))
import System.IO

main = deleteTodo

deleteTodo = do
    putStrLn "Which item would you like to delete?"
    numberString <- getLine
    contents <- readFile "todo.txt"
    
    _ <- evaluate (length contents)
    
    let items = zip [0..] (lines contents)
    let numberedItems = filter (\(i, item) -> i /= read numberString) items & map snd
    
    writeFile "todo.txt" (unlines numberedItems)

printTodo = do
    contents <- readFile "todo.txt"
    let items = zip [0..] (lines contents)
    let numberedItems = map (\(i, item) -> show i ++ " - " ++ item) items
    mapM_ putStrLn numberedItems

addTodo = do 
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

