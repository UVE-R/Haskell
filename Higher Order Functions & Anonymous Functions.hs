app :: (a->b) -> a ->b --higher order function 
app f x  = f x --applise the function onto itself

add1:: Int -> Int
add1 x = x+1

main = do
    print(app add1 4)

    let add2 = (\x -> x+1) --anonymous function to increment
    print(add2 1)

    print( (\x y z -> x+y+z) 1 2 3 ) --adds 3 numbers

    print( map (\x -> x+1) [1,2,3,4,5]) --increment each number in a list

    print( map (\(x,y) -> x+y) [(1,2), (2,3), (3,4)]) --add each tuple pair

    print(filter (\x -> x>2) [1,2,3,4,5]) --filter out all numbers less than 2
