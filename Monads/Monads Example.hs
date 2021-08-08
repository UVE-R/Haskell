
type Birds = Int --number of birds
type Pole = (Birds, Birds) --number of birds on the left and right pole

landLeft' :: Birds -> Pole -> Pole  
landLeft' n (left,right) = (left + n,right)  
  
landRight' :: Birds -> Pole -> Pole  
landRight' n (left,right) = (left,right + n)  

--add n birds to the left pole, if there is a difference > 3 then fall
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  --check if the difference is 3 or less
    | otherwise                    = Nothing  

--add n birds to the right pole, if there is a difference > 3 then fall
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  --check if the difference is 3 or less
    | otherwise                    = Nothing  

x -: f = f x --apply function by writing the parameter then function

main = do
    
    print(landLeft 2 (2,0)) -- =(4,0)

    print(landLeft' 2 (landRight' 1 (landLeft' 1 (0,0)))) -- =(3,1)

    print(100 -: (*3)) -- =300

    --landings in a more readable way
    print((0,0) -: landLeft' 1 -: landRight' 1 -: landLeft' 2) -- =(3,1)

    print(landLeft 2 (0,0)) -- =Just (2,0)
    print(landLeft 10 (0, 0)) -- =Nothing

    --cant do landLeft 2 (landRight 1 (0,0)) as landRight returns a Maybe Pole and landLeft requries a Pole
    print(landRight 1 (0,0) >>= landLeft 2) -- =Just (2,1)

    --need return to convert (0,0) into a monadic type
    print(return (0,0) >>= landRight 2 >>= landLeft 3 >>= landRight 2) -- =Just (2,4)

    --pole becomes offbalance
    print(return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)) -- =Nothing
