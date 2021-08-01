
--Add 3 numbers together
addThree :: (Num a) => a -> a -> a -> a  
addThree = \x -> \y -> \z -> x + y + z  --equivalent to x+y+z = 

-- flip the arguments around
flip' :: (a -> b -> c) ->b ->a ->c
flip' f = \x y -> f y x

--add the elements of a list using folds
sum':: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

--sum a list using a fold with no lambda function
sum'' :: (Num a) => [a] ->a
sum'' = foldl (+) 0

--elem using a left fold
elem' :: (Eq a) => a -> [a] ->Bool
elem' y ys = foldl (\acc x -> if x ==y then True else acc) False ys --if the values dont match, leave the acc as it is

--map using foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x acc -> f x:acc) [] xs --append x to the head of the accumulator

--maximum using foldr1
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x>acc then x else acc) --if x is greater, replace the acc, foldr1 sets acc to the rightmost element

-- length of square root sums
sqrtSums::Int 
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1,2..]))) +1

--sum of odd squre numbers
oddSquareSum :: Integer  
oddSquareSum =   
    let oddSquares = filter odd $ map (^2) [1..]  --get odd squares
        belowLimit = takeWhile (<10000) oddSquares  --get odd squares <10000
    in  sum belowLimit --add the squares

main = do

    -- Use a lambda function when zipping
    print(zipWith (\a b -> (a*30 +3)/b) [5,4,3,2,1] [1,2,3,4,5]) -- =[153.0,61.5,31.0,15.75,6.6]

    -- Use lambda function to add tuple pairs
    print(map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]) -- =[3,8,9,8,7]

    print(addThree 1 2 3) -- =6

    print( flip' (/) 4 2) -- =0.5

    print(sum' [1,2,3,4,5]) -- =15

    print(sum' [325,23,123,33]) -- =504

    print(elem' 5 [1,2,3,4,5,6]) -- =True

    print(map' (+2) [1,2,3,4,5]) -- =[3,4,5,6,7]

    print(maximum' [1,2,3,32,4,666]) -- =66

    --returns a list of the accumulator values when folding 
    print(scanl (+) 0 [1,2,3,4,5]) -- =[0,1,3,6,10,15]

    --final result is the head of the list
    print(scanr (+) 0 [1,2,3,4,5]) -- =[15,14,12,9,5,0] 

    --Number of elements needed for the sum of sqrts of natural numbers >1000
    print(sqrtSums) -- =131
    
    --apply 3 over a list of functions
    print(map ($ 3) [(4+), (10*), (^2), sqrt]) -- =[7.0,30.0,9.0,1.7320508075688772]

    --convert all numbers to negaative using function composition
    print(map (negate.abs) [5,-3,-6,7,-3,2,-19,24]  ) -- =[-5,-3,-6,-7,-3,-2,-19,-24]

    print(oddSquareSum) -- =166650
