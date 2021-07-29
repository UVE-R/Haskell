main = do
    print("Hello world " !! 0) --get element by index from a list

    print([[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]  !! 3 !! 2) --get the 3rd element from the 4th list

    print( [1,2,3,4] < [2,3,4,5]) --lexographically compare lists

    print( head [1,2,3,4]) --head of the list
    print( tail [1,2,3,4]) --list without the head 
    print( last [1,2,3,4]) --last element
    print( init [1,2,3,4]) --list without the last element

    print(take 2 [1,2,3,4]) --take n elements from the start of the list
    print(drop 2 [1,2,3,4]) --remove n elements from the start of the list
    
    print( 4 `elem` [3,4,5,6]) --checks if the item is present in the list
