--guards 
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  


bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height 
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 
    where bmi = weight / height^2    

--return a list of bmis
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

--maximum of 2 values
max' :: (Ord a) => a-> a-> a
max' a b 
    | a>b = a
    | otherwise = b

--compare 2 values
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  

--get initials from a name
initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l]
    where (f:_) = first
          (l:_) = last  

--cylinder surface area using let bindings
cylinder :: (RealFloat a) => a -> a -> a  
cylinder h r = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2*topArea

--case expressions to describe lists
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "Single item"
                                               xs -> "A longer list"  

main = do 
    print(bmiTell 11)
    print(bmiTell2 64 170)
    print(calcBmis [(70,180), (90,200), (54, 160)])
    print(max 1 2)
    print( 10 `myCompare` 8)
    print(initials "John" "Doe")
    print(cylinder 10 5)

    print([let square x = x * x in (square 5, square 3, square 2)]) --local function using let bindings

    print(describeList [1])
