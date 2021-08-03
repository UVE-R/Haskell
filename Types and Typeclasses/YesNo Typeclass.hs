class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False 
    yesno _ = True 

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id --id returns the same value as the parameter

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False  

--if the yesno of the value evaluated to true, return yesResult else return noResult
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  

main = do
    print(yesno $ length []) -- =False
    print(yesno "") -- =False
    print(yesno True) -- =True

    print( yesnoIf [] "YEAH!" "NO!") -- ="NO!"
    print(yesnoIf (Just 500) "YEAH!" "NO!") -- ="YEAH!"

