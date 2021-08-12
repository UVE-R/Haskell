
--list we are focusing on and the list of breadcrumbs
type ListZipper a = ([a],[a])  

goForward :: ListZipper a-> ListZipper a
goForward (x:xs, bs) = (xs, x:bs) --add the head of the list to the list of breadcrumbs

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b:bs) = (b:xs, bs) --add the head of the breadcrubs to the list to focus on

main = do
    
    let xs = [1,2,3]

    print(goForward (xs, [])) -- =([2,3],[1])

    print(goForward ([2,3,4],[1])) -- =([3,4],[2,1])

    print(goBackward ([4],[3,2,1])) -- =([3,4],[2,1])  

