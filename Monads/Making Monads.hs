import Data.Ratio
import Data.List

--new data type 
newtype Prob a = Prob { getProb :: [(a,Rational)]} deriving Show
--instance of functor
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  --unwrap, apply function then wrap back up

--probability list (a and b make up a quarter, a and b make half of that quarter)
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]  

--flatten a probability list
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  --apply multAll to each entry, concatenate the result and convert
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  --multiply the innerprob,r with the outer prob,p

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []  

--heads or tails data type
data Coin = Heads | Tails deriving Show

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]  

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

--probability that all the coins will land on tails
flipThree :: Prob Bool 
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])


main = do
    
    --rational numbers
    print(1%4) -- =1 % 4
    print(1%3 + 5%4) -- =19 % 12

    --flip signs
    print(fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])) -- =Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

    --get probability of each event
    print(flatten thisSituation) -- =Prob {getProb = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}
    
    print(getProb flipThree)
    {-
    [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
    (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]  
    -}
