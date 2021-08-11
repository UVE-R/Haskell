import Data.List  
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  

foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)  
  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result  

main = do
    print(solveRPN "1 2 * 4 +") -- =Just 6.0  

    print(solveRPN "1 8 wharglbllargh") -- =solveRPN "1 8 wharglbllargh"  
