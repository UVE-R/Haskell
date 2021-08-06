import Control.Applicative

{-
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
-}

{-
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
-}

{-
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
-}

sequenceA' :: (Applicative f) => [f a] -> f [a]  --list of applicatives to an appicative within a list
sequenceA' [] = pure []  
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs  --apply the applicative and append to the front of the list
                                                -- could use foldr (liftA2 (:)) (pure [])  

main = do
    
    --create a list which will multiply by the number in the original list
    let a = fmap (*) [1,2,3,4] 
    
    --apply the function in the list to 9
    print(fmap (\f -> f 9) a) -- =[9,18,27,36]  

    print(Just (+3) <*> Just 9 ) -- =Just 12
    print(Just 12) -- =Just 12
    print(Just (++"hahah") <*> Nothing) -- =Nothing (Mapping a function over Nothing)

    print(pure (+) <*> Just 3 <*> Just 5) -- =Just 8

    print( (++) <$> Just "johntra" <*> Just "volta") -- =Just "johntravolta"

    --apply each function seperately to the list
    print([(*0),(+100),(^2)] <*> [1,2,3] ) -- =[0,0,0,101,102,103,1,4,9]

    --all possible multiplications
    print((*) <$> [2,5,10] <*> [8,10,11]  ) -- =[16,20,22,40,50,55,80,100,110]

    --apply the two functions and add their resilts
    print((+) <$> (+3) <*> (*100) $ 5 ) -- =508

    --creates a list to store the result of each function
    print((\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5) -- =[8.0,10.0,2.5]

    --applies the function by index
    print(getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..] ) -- =[101,102,103]
    print(getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat") -- =[('d','c','r'),('o','a','a'),('g','t','t')]

    print((:) <$> Just 3 <*> Just [4]) -- = Just [3,4]

    print(sequenceA [(+3),(+2),(+1)] 3) -- =[6,5,4]

    print(map (\f -> f 7) [(>4),(<10),odd]) -- =[True,True,True]
    print(sequenceA [(>4), (<10), odd] 7) -- =[True,True,True]

    --all combinations of 2 lists
    print(sequenceA [[1,2,3],[4,5,6]] ) -- =[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  

    
