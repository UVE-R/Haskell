import Data.List 

--List generation function for integers within n to m inclusive
asc :: Int -> Int -> [Int] --return a list of integers
asc n m 
    | m<n = []
    | m == n = [m]
    | m>n = n: asc (n+1) m --call the function again with n+1


--function to sum elements of a list
sumL :: [Int] -> Int
sumL [] = 0 --sum of the empty list is 0
sumL (x:xs) = x + sumL xs --firts element added to the rest of the list

--function to return a list of even numbers from a list
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    | mod x 2 == 0  = x: evens xs --appends x to the list of evens from the rest of the list
    | otherwise = evens xs --carries on with the rest of the list

--function to add tuple pairs
addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs] 

main = do
    print("Lists")
    print(asc 1 3)
    print(head [1,2,3,4,5]) --head returns the first element of a list
    print(tail [1,2,3,4,5]) --tail returns a copy of the list list without the first element
    print(length [1,2,3,4,5]) --returns the length of a list
    print(init [1,2,3,4,5]) --returns a copy of the list withou the last element
    print(null []) --returns if a list is empty
    print(and [True, True, False]) --carry out boolean AND on a list
    print(or [True, False, True]) --carry out boolean OR on a list

    putStr "\n"

    print("List Comprehension")
    print([2*x | x <- [1,2,3]]) --doubles each element of the list
    print([2*x | x <- [1,2,3], x> 1]) --doubles each element of the list iff the element is greater than 1
    
    putStr "\n"

    print("List Patterns")
    print(sumL [1,2,3])
    print(evens [1,2,3,4,5,6,7,8])

    putStr "\n"

    print("Tuples")
    print(addTuples [(1,2), (5,10), (99,1)])
