
length' xs = sum [1 | _ <- xs]    --creates a new list of ones then sums them up to get the lenth

length'' (x:xs)   
    |xs == [] = 1
    |otherwise = 1+ length'' xs

removeNonUpperCase st = [c | c <-st, c `elem`  ['A'..'Z']] --returns the uppercase letters in a string

main = do
    --list ranges
    print([1..20]) --generate a list with all the number between 1 and 20 inclusive
    print(['a'..'z']) --list of the alphabet

    print([2,4..20]) --the step is the different between the first 2 specified elements

    print(['a','c'..'z']) --odd alphabet positions

    print(take 10 (cycle [1,2,3])) --create an inifite list of 1,2,3 and take the first 10 elements

    print(take 10 $ repeat 5) --create an infinite list of 5's and take 10 

    print(replicate 10 5) --a list with 10 5's

    print([x*2 | x <- [1..10], x*2 >=12]) --generate the first 10 even number which are >=12

    print([x | x<- [50..100], (x `mod` 7) == 3]) --number from 50-100 with remainder 3 when divided by 7

    let boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] --list comprehension   
    print(boomBangs [1..20])

    print([ x*y | x <- [1,2,3,4] , y<- [1,2,3,4]]) --list comprehension with 2 lists

    let nouns = ["Guy", "Dog", "Cat"]
    let adjectives = ["Friendly", "Creative", "Funny"]

    print( [adjective++ " " ++ noun  | noun <- nouns, adjective <- adjectives])

    print(length' [1,2,3])

    print(removeNonUpperCase "HHHHHHAAAa  aaahhhhaaaAAA")

    let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

    print( [ [x | x <- xs, even x] | xs <- xxs]) --remove the odd numbers in a 2D array

    print(fst (8,11)) --first of a tuple pair
    print(snd (8,11)) --second of a tuple pair

    print(zip [1,2,3,4] [5,6,7,8]) --combines the element of each list into a tuple pair

    --output all right angle triangles with perimeter 24
    let trianges = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
    print(trianges)
