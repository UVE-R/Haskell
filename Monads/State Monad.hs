import System.Random
import Control.Monad.State

--stack implementation
type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  --return the head and a stack without the head
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  --push to the top of the stack

stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  --push 3 
    (a ,newStack2) = pop newStack1  --pop 3, a is now 3
    in pop newStack2  

{- State Monad
newtype State s a = State { runState :: s -> (a,s) }  
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  
-}

pop' :: State Stack Int  
pop' = State $ \(x:xs) -> (x,xs)  
  
push' :: Int -> State Stack ()  
push' a = State $ \xs -> ((),a:xs)  

stackManip' :: State Stack Int  
stackManip' = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip' 
    if a == 100  
        then stackStuff'  
        else return ()  


{-
get = State $ \s -> (s,s)  
put newState = State $ \s -> ((),newState)  
-}

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  

--randomly toss 3 coins
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  --each state that is returned is passed onto the next random call
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)  

main = do
    
    print(stackManip [5,8,2,1]) -- =(5,[8,2,1])

    print(runState stackStuff [9,0,2,1,0]) -- =((),[8,3,0,2,1,0])  

    print(runState threeCoins (mkStdGen 33)) -- =((True,False,True),680029187 2103410263)  
