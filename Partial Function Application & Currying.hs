add1::Int->Int->Int

add1 x y = x+y --normal way

add2 x = (\y -> x+y) --1 anonymous function

--currying
--the function takes 1 argument x, which returns a function
--which takes 1 argument y which retirns x+y 
--"add3 1" would return (\y -> x+y)), generating a new function(partial function application)
add3 = (\x -> (\y -> x+y)) 

main = do
    print(add1 1 2)
    print(add2 1 2)
    print(add3 1 2)

    --map takes in a function and a list and outputs a list
    -- doubleList is now a function which takes in 1 list and returns another list (doubled)
    let doubleList = map(\x -> 2*x) 
    print(doubleList [1,2,3])
    
    
