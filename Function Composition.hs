import Data.List

--(.) :: (b -> c) -> (a ->b) ->a ->c
--using this, (f.g) is equal to (\x f(g x))

descSort = reverse . sort --first sorts a list ascendingly then reverses it
descSort2 = (\x -> reverse(sort x))
descSort3 x = reverse (sort x)

-- $ allows for the brackets to be removed
f xs = map (\x -> x+1) (filter (\x -> x>1) xs)
f2 xs = map (\x -> x+1) $ filter (\x -> x>1) xs

main = do
    print(descSort [8,9,3,12])
    print(descSort2 [8,9,3,12])
    print(descSort3 [8,9,3,12])
    
    print( f [1,2,4,6,7])
    print( f2 [1,2,4,6,7])
    
