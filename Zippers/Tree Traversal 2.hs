--tree is empty or has a node with 2 subtrees
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

--tree
freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

data Direction = L | R deriving (Show)
type Breadcrumbs = [Direction]  

--take a tree and breadcrumbs, then add directions taken to the head of a list
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft(Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
goRight (Node _ _ r, bs) = (r, R:bs)  

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x  

main = do

    --go right then left in a tree
    --return the directions taken as Breadcrumbs
    print(goLeft $ goRight (freeTree, [])) -- =(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

    --equivalent
    print((freeTree, []) -: goRight -: goLeft  ) -- =(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
