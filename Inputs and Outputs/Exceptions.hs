import System.Environment  
import System.IO  
import System.IO.Error  
import System.Directory
import Control.Exception


toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  --only catch does not exist errors
    | otherwise = ioError e  
  
main = toTry `catch` handler  

    {-
    --calculatee the number of lines in a file
    (fileName:_) <- getArgs            
    fileExists <- doesFileExist fileName  --check if the file exists
    if fileExists  
        then do contents <- readFile fileName  
                putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
        else do putStrLn "The file doesn't exist!"
    -}

     

