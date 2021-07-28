--count the occurences of an element within a list
count e = foldr (\x acc -> if e==x then acc+1 else acc) 0

--check if all the elements in a list are the same
isAll e = foldr( \x acc -> x==e && acc ) True

--calculate the length of a list
lengthL = foldr( \x -> (+)1) 0

--apply a function to map the elements of a list to another
mapf f = foldr((:) . f )[]


main = do
    print(foldr (+) 0 [1,2,3,4,5]) --folding to sum the numbers of a list
    
    print(count 1 [1,2,3,4,1])

    print(isAll 1 [1,1,1,1,1,1])
    print(isAll 2 [1,1,1,1,1,1])

    print(lengthL [1,2,4,45,5])

    print(mapf  (\x -> x*2) [1,2,3,4])
