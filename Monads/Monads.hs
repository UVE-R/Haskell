
{- Monad typeclass
class Monad m where  
    return :: a -> m a  --wraps a value in a monad
  
    (>>=) :: m a -> (a -> m b) -> m b  --Bind :takes a monadic value and a function which turns a normal value into a monadic value, then returns a monadic value
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg  
-}

{- Maybe monadic instance
class Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = nothing
-}

main = do
    
    --take in monadic maybe, a function which multiplies a normal value by 9 then turns it into a monadic value(using return), then returns the monadic value
    print(Just 9 >>= (\x -> return (x*9))) -- =Just 81

    print(Nothing >>= (\x -> return (x*10))) -- =Nothing

