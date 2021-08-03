--define a new data type
data Calculation = 
    Add Int Int | Sub Int Int | Mul Int Int | Div Int Int

--operations
calc :: Calculation -> Int
calc (Add x y) = x+y
calc (Sub x y) = x-y
calc (Mul x y) = x*y
calc (Div x y) = div x y

data PeaNum = 
    Succ PeaNum | Zero

incr = Succ

decr (Succ n) = n

main = do
    print(calc (Sub 1 2))
