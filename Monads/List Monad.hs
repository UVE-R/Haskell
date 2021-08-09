
{- List monad
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []  
-}

{-
-- MonadPlus typeclass
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  

instance MonadPlus [] where  
    mzero = []  
    mplus = (++) 

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero 

-}

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  


main = do

    print( [3,4,5] >>= (\x -> [x,-x])) -- =[3,-3,4,-4,5,-5]

    print([] >>= \x -> ["bad","mad","rad"]) -- =[]

    print([1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)) -- =[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  

    print(listOfTuples) -- =[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

    
