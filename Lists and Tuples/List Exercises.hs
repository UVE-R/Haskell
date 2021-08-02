import Data.List 

--Functions to return if an element is in a list
elem1 :: (Eq a) => a -> [a] -> Bool
elem1 n li
    | li == [] = False
    | (head li) == n = True
    | otherwise = elem1 n (tail li) 

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 _ [] = False
elem2 e (x:xs) = (e==x) || (elem2 e xs)


--Function to remove duplicates from a list
nub1 :: (Eq a) => [a] -> [a]
nub1 (x:xs)
    | xs == [] = x:[]
    | (elem2 x xs) = nub1 xs
    | otherwise = x: nub1 xs

--Functions to return if a list is ascending
isAsc :: [Int] -> Bool
isAsc [] = False
isAsc (x:xs)
    | xs == [] = True
    | x <= head(xs) = True && isAsc(xs)
    | otherwise = False

isAsc2 :: [Int] -> Bool
isAsc2 [] = False
isAsc2 [x] = True
isAsc2(x:y:xs) = 
    (x<=y) && isAsc (y:xs)


--Function to check if there is a path between 2 given nodes in a DIRECTED graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x==y --if there are no edges, check if the starting node is the end node
hasPath xs x y
    | x==y = True --node found
    
    --xs' is a new list of tuples from the original list where the starting node is NOT the node we previously came from
    --this is to stop infinite recursion if the graph is cyclic
    | otherwise = 
        let xs' = [ (n,m) | (n,m) <- xs, n/=x] in 
            or [ hasPath xs' m y | (n,m) <- xs, n==x] --start searching from all nodes connected to x in xs (m is the node connected to x)
                                                        --we use the boolean OR as if any of these calls returns true then there is a path

main = do
    print(elem1 5 [1,2,3] )
    print(elem2 5 [1,2,3] )

    print(nub1 [1,1,2,2,2,3,3,5])
    print(nub1 [1,1,2,2,2,3,3,5,12,22,12])

    print(isAsc [1,2,2,3,4,5])
    print(isAsc [])
    print(isAsc [1])

    print(hasPath [(1,2),(2,3),(3,2),(4,3),(4,5)] 1 5)
    print(hasPath [(1,2),(2,3),(3,2),(4,3),(4,5)] 1 3)
