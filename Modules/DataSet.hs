import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set


main = do 

    --text  
    let text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
    let text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  

    --convert text to set of characters
    let set1 = Set.fromList text1  
    let set2 = Set.fromList text2  
    
    --find shared characters
    print(Set.intersection set1 set2) -- =fromList " adefhilmnorstuy"

    --charcters in set1 but not in set2
    print(Set.difference set1 set2) -- =fromList ".?AIRj"

    --characters in set2 but not in set1
    print(Set.difference set2 set1) -- =fromList "!Tbcgvw"

    -- find unique letters used in both sets
    print(Set.union set1 set2) -- =fromList " !.?AIRTabcdefghijlmnorstuvwy"

    --set1 is a subset of set2 is set2 contains all of set1's elements
    print(Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]) -- =True

    --set1 is a subset of set2 is set2 contains all of set1's elements and extra elements
    print(Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]) -- =False

    
