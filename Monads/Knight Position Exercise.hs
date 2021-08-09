import Control.Monad

type KnightPos = (Int,Int)

--take a knight position and return all of its next moves
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  --check if the move is still on the board
    return (c',r')  

--apply 3 moves
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

--checks if you can reach a position in 3 steps
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  --check if end is an element of all the reachable squares

main = do
    
    print(moveKnight (6,2)) -- =[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]

    print((6,2) `canReachIn3` (6,1)) -- =True

    print((6,2) `canReachIn3` (7,3)) -- =Fasle

