import Data.Monoid
import Control.Monad.Writer

isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog(x,log) f = 
    let (y,newLog) = f x
    in (y,log `mappend` newLog)  

type Food = String 
type Price = Sum Int 

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99) 
addDrink _ = ("beer", Sum 30)  

logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])    
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  


gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  

finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  

main = do

    print((3, "Smallish gang.") `applyLog` isBigGang) -- =(False,"Smallish gang.Compared gang size to 9.")

    print(("beans", Sum 10) `applyLog` addDrink) -- =("milk",Sum {getSum = 35})

    print(runWriter multWithLog) -- =(15,["Got number: 3","Got number: 5"])  

    print(mapM_ putStrLn $ snd $ runWriter (gcd' 8 3))
    {- = 8 mod 3 = 2  
        3 mod 2 = 1  
        2 mod 1 = 0  
        Finished with 1  
    -}

    --counts upwards from 0 to 100
    print(mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 100)
