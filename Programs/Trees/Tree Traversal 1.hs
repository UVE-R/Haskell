

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

--change the W in the tree to a P
{-
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node "P" m n) r)
-}

data Direction = L | R deriving (Show)
type Directions = [Direction]

--given a list of directions, follow those directions to a node and change the letter to a P
changeToP :: Directions -> Tree Char -> Tree Char 
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r --if the direction is left then call again with the left tree and tail of the list
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r) --if the direction is right then call again with the right tree and tail of the list
changeToP [] (Node _ l r) = Node 'P' l r  --if there are no more directions, then we have got to the node to change

--tells us the element of the tree
elemAt :: Directions -> Tree a -> a
elemAt(L:ds) (Node x l r) = elemAt ds l
elemAt(R:ds) (Node x l r) = elemAt ds r
elemAt [] (Node x l r) = x

main = do

    let newTree = changeToP [R,L] freeTree  --change the W to a P
    print(elemAt [R,L] freeTree) -- ='W'
    print(elemAt [R,L] newTree) -- ='P'

    
