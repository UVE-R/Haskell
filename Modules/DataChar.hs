import Data.List
import Data.Char
import Data.Function


--caesar cipher
encode::Int -> String ->String 
encode shift msg =
    let ords = map ord msg --get the unicode representation for each character
        shifted = map (+ shift) ords --shift the unicode numbers 
    in map chr shifted --convert from unicode to character

--decode caesar cipher
decode :: Int -> String ->String 
decode shift = encode (negate shift) --undo the shifts using opposite parity shift

main = do

    --check if all characters are alphanumerical
    print(all isAlphaNum  "bobby283") -- =True
    
    --simulate words function
    print(groupBy ((==) `on` isSpace) "hey guys its me") -- =["hey"," ","guys"," ","its"," ","me"]

    --convert each element from a character to an int
    print(map digitToInt "34538") -- =[3,4,5,3,8] 

    --convert an int to a lower case character
    print(intToDigit 15)

    --convert character to number
    print(ord 'a') -- =97

    --convert number to character
    print(chr 97) -- ='a'

    --encode a message
    print(encode 5 "Merry Christmas! Ho ho ho!") -- ="Rjww~%Hmwnxyrfx&%Mt%mt%mt&"

    --decode a message
    print(decode 5 "Rjww~%Hmwnxyrfx&%Mt%mt%mt&") -- ="Merry Christmas! Ho ho ho!"
