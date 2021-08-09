
--is 9 greater than 8
--equivalent to Just 9 >>= (\x -> Just (x > 8))  
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)  

--pattern match in a do block
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x  

wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  

main = do

    --both are the same but the first is monadic
    print(Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))) -- =Just "3!"
    print(let x = 3; y = "!" in show x ++ y) -- ="3!"

    print(marySue) -- =Just True
    
    print(justH) -- =Just 'h'

    --when pattern matching fails, instead of crashing it returns nothing
    print(wopwop) -- =Nothing
