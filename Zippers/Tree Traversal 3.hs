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
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x  

data Direction = L | R deriving (Show)

--Crumb contains the node we moved from and the left/right tree we did not visit
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]  

--move through the tree, adding the path as Breadcrumbs
goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = goLeft (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)  

--move up the tree
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs) --construct a new tree with tree t being the left subtree
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)  

--modify the element in the root of a subtree
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r,bs)
modify f (Empty,bs) = (Empty,bs)

--attack a tree to a zipper
attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)  

--go to the root of the tree
topMost ::  Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost $ goUp z --call the function again from the parent node


main = do

    --go left, then right, then change the root to a P
    let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')  

    --go up the tree and change to X
    let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')      

    --go to the left-most node
    let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
    --add Z to that node
    let newFocus = farLeft -: attach (Node 'Z' Empty Empty)  

