import Data.Char  
import Data.List 
  
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  --apply the function to the second field and increment the first field

main = do 

    --fmap acts as function composition
    print(fmap (*3) (+100) 1) -- =303

    print(fmap (++ "haha") (CJust 0 "ho")) -- =CJust 1 "hohaha"
