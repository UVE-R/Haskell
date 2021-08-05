import System.Random  
import Control.Monad(when)  

--get the user to guess the number between 1 and 10 inclusive

main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  --generate random number
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  --get input
    when (not $ null numberString) $ do  
        let number = read numberString  --conver to int
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen  --repeat with the new generator
