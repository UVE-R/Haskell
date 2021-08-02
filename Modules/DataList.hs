import Data.List

--calculate the number of unique elements in a list
numUnique:: (Eq a) => [a] -> Int 
numUnique = length . nub --num removes duplicate elements from a list

--search a list for a sublist
search :: (Eq a) => [a] -> [a] -> Bool 
search needle haystack = 
    let nlen = length needle
    in foldl(\acc x -> if take nlen x == needle then True else acc) False (tails haystack) --go over each tail 

main = do
    print(numUnique [1,2,1,5,6]) -- =4

    --intersperse places the element between each pair of elements
    print(intersperse '.' "HELLO") -- ="H.E.L.L.O"

    --intercalate inserts a list between lists
    print(intercalate " " ["Hello", "My", "Friends"]) -- ="Hello My Friends"
    print(intercalate [0,0] [[1,2,3],[4,5,6],[7,8,9]]) -- =[1,2,3,0,0,4,5,6,0,0,7,8,9]

    --transpose transposes a matrix (inverts the rows and columns)
    print(transpose [[1,2,3],[4,5,6],[7,8,9]]) -- =[[1,4,7],[2,5,8],[3,6,9]]
    print(transpose ["hey","there","guys"]) -- =["htg","ehu","yey","rs","e"]]

    --turns a list of lists into a list of elements
    print(concat ["foo","bar","car"]) -- ="foobarcar"
    print(concat [[3,4,5],[2,3,4],[2,1,1]]) -- =[3,4,5,2,3,4,2,1,1]

    --apply a function to individual elements then concanenate the list
    print(concatMap (replicate 4) [1,2,3]) -- =[1,1,1,1,2,2,2,2,3,3,3,3]

    --check if any element meets the predicate
    print(any (==2) [1,3,4,5,2]) -- =True
    
    --check if all elements meets the predicate
    print(all (==2) [2,2,2,2,2]) -- =True

    --iterate infinitely carries out a function on a starting value
    print(take 10 $ iterate (*2) 1) -- =[1,2,4,8,16,32,64,128,256,512]  

    --split a list after n elements
    print(splitAt 3 "Hello there") -- =("Hel","lo there")

    --take elements from a list while the predicate holds
    print( takeWhile (>3) [6,5,4,3,2,1]) -- =[6,5,4]

    --remove elements from the list while the predicate holds
    print(dropWhile (>3) [6,5,4,3,2,1]) -- =[3,2,1]

    --split the list where the predicate is first true
    print(break (==4) [1,2,3,4,5,6,7]) -- =([1,2,3],[4,5,6,7])

    --split the list into the taken elements and the removed elements
    print(span (/=4) [1,2,3,4,5,6,7]) -- =([1,2,3],[4,5,6,7])

    --group adjacent elements into sublists
    print(group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]) -- =[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

    --recursively apply init 
    print(inits "w00t") -- =["","w","w0","w00","w00t"]

    --recursively apply tail
    print(tails "w00t") -- =["w00t","00t","0t","t",""]

    print(search [1,2,3] [23,2,1,2,3,4]) -- =True

    --seach for a sublist within a list
    print("cat" `isInfixOf` "Hello Mr cat") -- =True

    --search for a sublist at the start of a list
    print("hey" `isPrefixOf` "hey there!")

    --search for a sublist at the end of a list
    print("there!" `isSuffixOf` "oh hey there!")

    --go through the ENTIRE list and split according to the predicate
    print(partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" ) -- =("BOBMORGAN","sidneyeddy")

    --returnt the first element which satisfies the predicate
    print(find (>4) [1,2,3,4,5,6]  ) -- =Just 5
    print(find (>6) [1,2,3,4,5,6]  ) -- =Maybe

    --return the index which the element is at
    print(4 `elemIndex` [1,2,3,4,5,6] ) -- =Just 3

    --return the index at which the predicate is first true
    print(findIndex (==4) [5,3,2,1,6,4]  ) -- =Just 5

    -- split a line of text into individual words in a list
    print( words "hey these are the words in this sentence" ) -- =["hey","these","are","the","words","in","this","sentence"]

    --join a list of words into text
    print(unwords ["hey","there","mate"] ) -- ="hey there mate"

    --delete the first occurence of an element from a list
    print(delete 'h' "hey there ghang!" ) -- ="ey there ghang!"

    --set difference
    print([1..10] \\ [2,5,9] ) -- =[1,3,4,6,7,8,10]  

    --set union
    print([1..7] `union` [5..10]) -- =[1,2,3,4,5,6,7,8,9,10] 

    --set intersection
    print([1..7] `intersect` [5..10]) -- =[5,6,7]

    --insert an element in front of the first element which is greater than it
    print(insert 4 [3,5,1,2,8,2]) -- =[3,4,5,1,2,8,2]

    
