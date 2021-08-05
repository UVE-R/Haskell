  
import System.IO
import System.Directory  
import Data.List 

data Node = Node Road Road | EndNode Road --road
data Road = Roat Int Node

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]  

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  

--find the best path along a and along b within each section
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  --optimal price along a
        priceB = sum $ map snd pathB  --optimal price along b
        forwardPriceToA = priceA + a  --going straight along from a
        crossPriceToA = priceB + b + c  --cross from current b to the next a
        forwardPriceToB = priceB + b  --going straight along from b
        crossPriceToB = priceA + a + c  --cross from current a to the next b
        newPathToA = if forwardPriceToA <= crossPriceToA  --best way to go to the next a 
                        then (A,a):pathA  --add the new path to the front of the paths
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  --best way to go to the next b
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  

--find the optimal path
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem --apply roadStep to each section
    in if sum(map snd bestAPath) <= sum(map snd bestBPath) --calculate the shortest path out of a and b
        then reverse bestAPath
        else reverse bestBPath

--split the input into groups of size n
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs) --take n values and then call the function again without those values

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

main = do     

    print(roadStep ([],[]) (head heathrowToLondon)) --([(C,30),(B,10)],[(B,10)])

    print(optimalPath heathrowToLondon) -- =[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]

    
    handle <- openFile "path.txt" ReadMode --open file
    contents <- hGetContents handle --get file contents
    let threes = groupsOf 3 (map read $ lines contents) --split the input into a 2d list of groups of three
        roadSystem = map (\[a,b,c] -> Section a b c) threes --convert the triples into a Section
        path = optimalPath roadSystem --find the best path
        pathString = concat $ map (show . fst) path  --convert the path to a string
        pathPrice = sum $ map snd path  -- get the price
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice  
