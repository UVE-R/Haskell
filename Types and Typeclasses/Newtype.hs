
newtype Pair b a = Pair { getPair :: (a,b) } 
--only apply the function to the first element of the tuple 
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  


--create a data type from a pre-existing one
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  

main = do
    
    print(CharList "Hello World") -- =CharList {getCharList = "Hello World"}
    let test = CharList "Hello World"
    print( getCharList test) -- ="Hello World"

    print(getPair $ fmap (*100) (Pair (2,3))) -- =(200,3)
