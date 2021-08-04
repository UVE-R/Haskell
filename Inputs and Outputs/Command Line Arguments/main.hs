import System.Environment   
import System.Directory  
import System.IO 
import Data.List  
import Control.Monad.Catch (handleIOError)
  
--dispatch list
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  


--add an item to the file
add:: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") 

--view the items of the file
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName 
    let todoTasks = lines contents 
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

--remove an item from the file
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  

--moves the task at numberString to the first task
bump :: [String] -> IO ()
bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode 
    contents <- hGetContents handle 
    
    let taskNum = read numberString
        allTasks = lines contents
        task =  allTasks !! taskNum --get the task to bump
        newContents = delete task allTasks --remove the task from the list

    let taskList = task : newContents --place the task at the top of the list

    (tempName, tempHandle) <- openTempFile "." "temp"  
    
    hPutStr tempHandle $ unlines taskList  --add the new list to the temp file
    
    hClose handle
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

main = do

    {-
    (command:args) <- getArgs --bind the command and arguments
    let (Just action) = lookup command dispatch --lookup the command in the dispatch list
    action args --call the action function with the arguments   
    -}

    view ["todo.txt"]
    putStr "\n"
    
    add ["todo.txt", "Do the ironing"]
    putStr "\n"
    view ["todo.txt"]
    
    remove["todo.txt", "1"]
    putStr "\n"

    
    view ["todo.txt"]

    bump["todo.txt", "3"]

    putStr "\n"
    view ["todo.txt"]
    
