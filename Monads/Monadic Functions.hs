import Control.Monad (liftM, join, filterM, foldM)
import Control.Monad.Trans.Writer


{- liftM monad
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))  
-}

-- ap monad
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
    f <- mf  
    x <- m  
    return (f x)  


--keeps a log of the results
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs --choose to both drop and keep each element (filterM will drop if function is true)

--add a value to the accumulator, if the value is >9 then fail
binSmalls :: Int -> Int -> Maybe Int 
binSmalls acc x 
    | x>9 = Nothing
    | otherwise = Just (acc +x)

main = do

    --apply function to monadic value
    print(liftM (*3) (Just 8)) -- =Just 24  

    --apply function to first part of writer
    print(runWriter $ liftM not $ writer (True, "chickpeas") ) -- =(False,"chickpeas")

    --equivalent
    print(Just (+3) <*> Just 4) -- =Just 7
    print(Just (+3) `ap` Just 4) -- =Just 7

    --flatten a monadic value
    print(join (Just(Just 9))) -- =Just 9

    --flatten a 2d array into a 1d array
    print(join [[1,2,3],[4,5,6]]) -- =[1,2,3,4,5,6]
    print(concat [[1,2,3],[4,5,6]]) -- =[1,2,3,4,5,6]

    -- bbb will come first
    print(runWriter $ join (writer (writer (1,"aaa"),"bbb"))) -- =(1,"bbbaaa")

    --apply keepSmall to each element of the list, take value of the writer
    print(fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]) -- =[1,2,3]

    --apply keepSmall to each element of the list, take log of the writer and output on seperate lines
    mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3] 
    {-
        9 is too large, throwing it away
        Keeping 1
        5 is too large, throwing it away
        Keeping 2
        10 is too large, throwing it away
        Keeping 3
    -}

    --all the sublists of a list
    print(powerset [1,2,3])

    --sum a list using foldl
    print(foldl (\acc x -> acc + x) 0 [1,2,3,4]) -- =10

    print(foldM binSmalls 0 [2,3,4,5]) -- =Just 14
    print(foldM binSmalls 0 [2,11,3,1]) -- =Nothing
    
    
