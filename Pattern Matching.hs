--pattern matching
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"   

--factorial function
fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)

--add vectors together
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--get element from a tuple with 3 elements
first :: (a,b,c) -> a
first (a,_,_) = a 

second :: (a,b,c) -> b
second (_,b,_) = b 

third :: (a,b,c) -> c
third (_,_,c) = c

--return the head of a list
head' :: [a] -> a 
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x 

--return the length of a list
length' :: (Num b) => [a] -> b 
length' [] = 0
length' (_:xs) = 1 + length' xs --call the function with the tail of the list


--return the sum of a list
sum' :: (Num a) => [a] ->a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--output the first letter of a string
capital :: String -> String
capital "" = "Empty String, Whoops!"
capital all@(x:xs) = "The First letter of " ++ all ++ " is " ++ [x] --all will store the entire list before pattern matching

main = do 
    print(sayMe 1)
    print(sayMe 7)
    print(fact 4)

    print(addVectors (1,2) (3,4))
    
    print(first (1,2,3))
    print(second (1,2,3))
    print(third (1,2,3))

    print(head' [8,21,37])

    print(length' [1,2,3,45])

    print(sum' [1,2,3,4,5])

    print(capital "Hello")
