import Data.List
import Data.Char
import qualified Data.Map as Map

--return the corresponding value to the key
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing  --set the accumulator to the value if the key matches

--Map.fromList implementation
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty  --insert key value pairs into the empty map

-- return values from all keys including duplicates
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs   --fromListWith will combine duplicate keys

main = do   
    
    let phoneBook = [("betty","555-2938") ,("bonnie","452-2928") ,("patsy","493-2928") ,("lucille","205-2928")] --association list
    print(findKey "patsy" phoneBook) -- =Just "493-2928"

    --take an association list and convert it into a map
    print(Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")])

    --insert a new key-value into a map
    print(Map.insert 3 100 Map.empty ) -- =fromList [(3,100)]

    --check if a map is empty
    print(Map.null Map.empty) -- =True

    --output the size of a map
    print(Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]) -- =5

    --check if the key is in the map
    print(Map.member 3 $  Map.fromList [(3,6),(4,3),(6,9)]) -- =True

    -- association lis with repeated keys
    let phoneBook = [("betty","555-2938"),("betty","342-2492"),("bonnie","452-2928"),("patsy","493-2928"),("patsy","943-2929"),("patsy","827-9162"),("lucille","205-2928"),("wendy","939-8282"),("penny","853-2492"),("penny","555-2111")]  

    print(Map.lookup "patsy" $ phoneBookToMap phoneBook) -- =Just "827-9162, 943-2929, 493-2928"

    --insert a key value, if the key already exists then apply the function passed on the value
    print( Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]) -- =fromList [(3,104),(5,103),(6,339)]
