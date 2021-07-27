in_range :: Integer -> Integer -> Integer -> Bool --declare types
--function to check if a number is within bounds
in_range min max x = 
    x>=min && x<=max

--function using binding 
in_range2 min max x = 
    let in_lower_bound = min <= x
        in_upper_bound = max >= x
    in
    in_lower_bound && in_upper_bound


--function using where binding
in_range3 min max x = ilb && iub 
    where
        ilb = min <= x
        iub = max >= x

--function using if then else
in_range4 min max x =
    if ilb then iub else False --works like bool and 
    where
        ilb = min <= x
        iub = max >= x

add a b = a+b --add 2 number

main = do
    print(in_range4 10 20 15)
    print(10 `add` 20) --infix style
    print(add 10 20)
