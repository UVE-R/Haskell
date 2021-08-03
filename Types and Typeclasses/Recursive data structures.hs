
--binary search tree
import GHC.Conc (numSparks)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--creates a tree with only 1 node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree --a single node which connects to two empty subtrees

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x --once reaching an empty subtree, insert the singleton
treeInsert x (Node a left right)
    | x == a = Node x left right --return the same tree
    | x < a = Node a (treeInsert x left) right --if x is less than a, follow the tree to the left
    | otherwise = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x ==a = True
    | x > a = treeElem x right
    | otherwise = treeElem x left

main = do
    let nums = [8,6,4,1,7,3,5]
    let numsTree = foldr treeInsert EmptyTree nums -- create a tree by folding the list from the right
    print(numsTree) -- =Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

    --check if 8 is a member of the tree
    print(8 `treeElem` numsTree) -- =True

    print(10 `treeElem` numsTree) -- =False
