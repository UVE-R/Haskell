
--multiply 3 numbers together
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

--divide a number by 10
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  

--check if a character is uppercase
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

--apply a function twice
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

--apply a function between 2 corresponding elements in a list
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--return a function which is the orgignal function but with the parameters flipped
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  

--apply a function to all elements of a list
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

--filter a list to meet a certain condition
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)
    | p x = x: filter' p xs
    | otherwise = filter' p xs

--quicksort using filter function
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort(filter' (<=x) xs) --filter out elements less than or equal to the pivot and sort them
        biggerSorted = quicksort(filter' (>x) xs) --filter out elements greater than the pivot and sort them
    in smallerSorted ++ [x] ++ biggerSorted

--largest number divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head(filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

--sum of the odd square numbers <= 10000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (filter p (map (^2) [1,2..100]))
    where p x = odd x &&  x<10000

--Collatz sequences with length >15 starting between 1 and 100
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n: chain(n `div` 2)
    | odd n = n:chain(n*3 +1)

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100] )) 
    where isLong xs = length xs>15 --lists with 15 or more elements

main = do
    print(max 4 5)
    print( (max 4) 5) --curried function

    let multTwoWithNine = multThree 9 --create a new function which multiplies 2 numbers with 9
    print(multTwoWithNine 2 3) -- =54

    print(divideByTen 200) -- = 20.0

    print(isUpperAlphanum 'A') -- =True

    print(applyTwice (+3) 10) -- =16

    print(zipWith' (/) [32,46,64,9] [4,2,8,3]) -- =[8.0,23.0,8.0,3.0]

    print(flip' (/) 4 2 ) -- =0.5

    print(map' (/2) [2,4,6,8,10]) -- =[1.0,2.0,3.0,4.0,5.0]

    print(filter' (>=10) [1,3,45,10,32]) -- =[45,10,32]

    print(quicksort [1,4356,58,1232,3,123]) -- =[1,3,58,123,1232,4356]

    print(largestDivisible) -- =99554

    print(sumOddSquares) -- =166650

    --sum of the odd squares <=10000
    --takeWhile will return elements for which the condition holds true
    print(sum (takeWhile (<10000) (filter odd (map (^2) [1..])))) -- =166650

    print(chain 10) -- =[10,5,16,8,4,2,1]

    print(numLongChains) -- =66

