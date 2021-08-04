import System.IO
import Data.Char
import System.Directory  
import Data.List  


--check if each line is a palindrome
respondPalidromes :: String -> String
respondPalidromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines where isPalindrome xs = xs == reverse xs

main = do
    
    --test for palindrome
    putStr $ respondPalidromes "dad\nhello\nracecar"
    putStr "\n"

    handle <- openFile "text.txt" ReadMode --open a file
    contents <- hGetContents handle --get the contents of the file
    putStr contents --output the contents
    hClose handle --close the file

    putStr("\n\n")
    
    withFile "text.txt" ReadMode (\handle -> do
        contents <- hGetContents handle --get the contents
        putStr contents) --print the contents

    
    handle <- openFile "todo.txt" ReadMode  --open file
    (tempName, tempHandle) <- openTempFile "." "temp"  --open a temporary file in the same directory
    contents <- hGetContents handle  --get contents
    
    let todoTasks = lines contents  --split into a list of strings   
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  --pair each line with its corresponding number from 0   
    
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    
    let number = read numberString     --turn the input number string to an integer
        newTodoItems = delete (todoTasks !! number) todoTasks     --delete the occurence from the list
    hPutStr tempHandle $ unlines newTodoItems  --write to the temporary file
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"  --replace the file


