--recursive factorial function using if then else
fac n =
    if n<=1 then
        1
    else
        n * fac(n-1)

--recursive factorial function using guards
fac2 n 
    | n<=1 = 1
    | otherwise = n * fac2(n-1)

-- tail recursive factorial using accumulators
fact3 n = aux n 1
    where
        aux n acc
            | n<=1 = acc --return the result at the current point
            | otherwise = aux (n-1) (n*acc) --update n to n-1 and update acc with acc*n


--Pattern matching to check if zero
is_zero 0 = True
is_zero _ = False

main = do
    print(fac2 4)
    print(fact3 4)
    print(is_zero 0)
    print(is_zero 1)
