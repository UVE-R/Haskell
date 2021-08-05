import Data.List

--reverse polish notation
solveRPN :: String -> Float   
solveRPN = head . foldl foldingFunction [] . words  --go over the list from left to right and apply the folding function on the stack ([])
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  --define operations
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs  --if it is a number then add it to the head of the list

main = do
    print(solveRPN "10 4 3 + 2 * -") -- =-4
    print(solveRPN "90 34 12 33 55 66 + * - + -") -- =4037
    print(solveRPN "2.7 ln") -- =0.9932518
    print(solveRPN "10 2 ^") -- =100.0  

    
