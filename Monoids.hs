import Data.Monoid
import qualified Data.Foldable as F  

{- Monoid TypeClass
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
-}

{- LIST monoid
instance Monoid [a] where  
    mempty = []  
    mappend = (++)  
-}


{- PRODUCT monoid

newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
-}

{- OR monoid
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  
instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)  
-}

{- AND monoid
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  
-}

{- ORDERING monoid
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT
-}

{- MAYBE monoid
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
-}

{- FIRST monoid
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  
-}

--compare the lengths of 2 strings, if they are equal compare them alphabetically
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  --recursively foldmap over the left and right subtrees
                             f x           `mappend`  
                             F.foldMap f r  

main = do
    
    
    print([1,2,3] `mappend` [4,5,6]) -- =[1,2,3,4,5,6]
    
    --apply a binary function to the monoids
    print("pang" `mappend` mempty) -- ="pang"
    
    --list of monoids to a single list by applying binary operation
    print(mconcat [[1,2],[3,6],[9]]) -- =[1,2,3,6,9]


    print(getProduct $ Product 3 `mappend` Product 9) -- =27

    --convert them all to type Any, apply binary function to each element
    print(mconcat . map Any $ [False, False, False, True]) -- =True  
    
    --convert them all to type All, apply binary function to each element
    print(getAll . mconcat . map All $ [True, True, True]) -- =True

    print(mempty `mappend` GT) -- =GT

    print(lengthCompare "zen" "ants") -- =LT

    print(lengthCompare "zen" "ant") -- =GT

    print(Nothing `mappend` Just "andy") -- ="Just andy"

    print(Just (Sum 3) `mappend` Just (Sum 4)) -- =Just (Sum {getSum = 7})

    print(getFirst $ First (Just 'a') `mappend` First (Just 'b')) -- =Just 'a'

    --check if any Maybe values are a just
    print(getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]) -- =Just 9  

    --fold on data types other than lists using monoids
    print(F.foldr (||) False (Just True)) -- =True  

    --binary tree
    let testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  

    --add all nodes
    print(F.foldl (+) 0 testTree) -- =42

    --multiply all nodes
    print(F.foldl (*) 1 testTree ) -- =64800  

    --check if 3 is in the tree
    print(getAny $ F.foldMap (\x -> Any $ x==3) testTree) -- =True

    --convert tree to list
    print(F.foldMap (\x -> [x]) testTree) -- =[1,3,6,5,8,9,10]
    
