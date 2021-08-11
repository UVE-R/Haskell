import Control.Monad

main = do

    let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  --multiply by 100 then add 1
    print(Just 4 >>= g) -- =Just 401

    let f = foldr (.) id [(+5),(*100),(+1)]  --combine all the functions into a single function
    print(f 1) -- = 205
