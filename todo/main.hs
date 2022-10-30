import System.Environment
import System.IO
import Control.Exception
import System.Directory
import Data.List

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = invalidCommandError command

invalidCommandError :: String -> [String] -> IO ()
invalidCommandError command _ = putStrLn $ "Your specified command '" ++ command ++ "' is not valid command."

main = do
    (command: argList) <- getArgs
    dispatch command argList
    

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"

view :: [String] -> IO ()
view [fileName] = do
     contents <- readFile fileName
     let todoTasks = lines contents
         numberedTasks = zipWith (\n line -> show n ++ " _ " ++ line) [0..] todoTasks
     putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " _ " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let number = read numberString
        todoItems = lines contents
    
    let targetItem = todoItems !! number
        newTodoItems = targetItem : (delete targetItem todoItems)
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName) 
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle  $ unlines newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
    
    putStrLn "These are your todo items"
    mapM_ putStrLn $ zipWith (\n item -> show n ++ " - " ++ item) [0..] newTodoItems
    