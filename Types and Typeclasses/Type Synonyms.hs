import qualified Data.Map as Map 

data LockerState = Taken | Free deriving (Show, Eq) --new datatype to represent if a locker is free or taken

type Code = String 

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of --look up in the map
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!" --if the locker number does not exist
        Just (state, code) -> if state /= Taken --if the locker does exist, first check if the locker is taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"  

main = do

    let lockers = Map.fromList   
            [(100,(Taken,"ZD39I"))  
            ,(101,(Free,"JAH3I"))  
            ,(103,(Free,"IQSA9"))  
            ,(105,(Free,"QOTSA"))  
            ,(109,(Taken,"893JJ"))  
            ,(110,(Taken,"99292"))  
            ]  

    print(lockerLookup 101 lockers) -- =Right "JAH3I"

    print(lockerLookup 100 lockers) -- =Left "Locker 100 is already taken!"
