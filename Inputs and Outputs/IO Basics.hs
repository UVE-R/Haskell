import Data.Char
import Control.Monad

reverseWords :: String -> String
reverseWords = unwords . map reverse . words --convert to a list of words, reverse each individual word, then put the word list into a string


main = do  

    --get first and last names
    putStrLn "What is your first name"
    firstname <- getLine 
    putStrLn "What is your last name"
    lastname <- getLine 

    --output message
    let firstnameUpper = map toUpper firstname
        lastnameUpper = map toUpper lastname 
    putStrLn $ "hey " ++ firstnameUpper ++ " " ++ lastnameUpper ++ " how are you?"


    {-
    line <- getLine 
    if null line 
        then return () --stop when nothing is entered
        else do
            putStrLn $ reverseWords line
            main --call the main function again
    -}


    --putStr does not jump to a new line
    putStr "Hello"
    putStr " World"


    {-
    --get each character from the input and output it
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  
    -}

    --sequence will perform the IO action one after another 
    sequence (map print [1,2,3,4]) 

    --print a list with no ""
    mapM print [1,2,3]  
    mapM_ print [1,2,3]  -- this will not return [(),(),(),()]


    {-
    --repeatedly ask the user for input then return it uppercase
    forever $ do
        putStr "Give me some input"
        l <- getLine 
        putStrLn $ map toUpper l
    -}

    colours <- forM [1,2,3,4] (\a -> do  --colours is a list which will hold the strings
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        colour <- getLine  --get input
        return colour)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM_ putStrLn colours  --print each colour from the list colours
