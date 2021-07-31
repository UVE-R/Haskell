
--find the maximum of the list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) 
    | x> maxTail = x --is the head larger
    |otherwise = maxTail 
    where maxTail = maximum' xs --calculate the maximum of the tail

--maximum of a list
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)  --maximum of tail

--replicate x n times
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x 
    | n<=0 = []
    | otherwise = x:replicate' (n-1) x

--return the first n elements from a list
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs --call the function again with the tail of the list

--reverse a list
reverse' :: [x] -> [x]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] --add x to the back of the reversed list

--zip 2 lists together
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = [] --check for empty lists
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

--check if an element  is within the list
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  --no more elements to check
elem' a (x:xs)  
    | a == x    = True  --item found
    | otherwise = a `elem'` xs   

--quicksort 
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort[ a | a<-xs, a<=x] --recursively sort the larger and smaller array
        biggerSorted = quicksort[ a | a<-xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted --x is the pivot 

main = do
    print(maximum [1,23,3])
    print(maximum [45,23,3])

    print(replicate' 5 2)
    print(take' 4 [1,2,3,4,5,6])

    print(reverse' [1,2,3,4,5,6])

    print(zip [1,2,3,4] [5,6,7,8])

    print(quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]  )
