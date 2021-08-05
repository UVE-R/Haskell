import System.Random

--flip 3 coins 
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  

--generate a finite stream of random numbers
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)  

main = do
    
    --calling this again will lead to the same output
    print(random (mkStdGen 100) :: (Int, StdGen)) -- =(9216477508314497915,StdGen {unStdGen = SMGen 712633246999323047 2532601429470541125})

    --using a different random generator
    print(random (mkStdGen 949494) :: (Int, StdGen)) -- =(-3721561930999131179,StdGen {unStdGen = SMGen 3529285061597105018 656261746462218851})

    --take 5 numbers from an infinite list of random number
    print(take 5 $ randoms (mkStdGen 11) :: [Int]) -- =[-8691282579076269733,-6401397088113744335,-7708536135073118066,1428399377092000640,-6279174627358091186]

    --generate a string of 2 random letters
    let gen = (mkStdGen 100)
    putStr $ take 20 (randomRs ('a','z') gen)  

    
