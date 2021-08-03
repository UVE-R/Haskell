--data car person using record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

--3d vector
data Vector a = Vector a a a deriving (Show)
--vector addition
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
--multiply by scalar
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
--multiply 2 vectors
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  


data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)  --Allow for equating

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

main = do

    let car = Car {company="Ford", model="Mustang", year=1967}  
    print(car) -- =Car {company = "Ford", model = "Mustang", year = 1967}

    --by using record syntax, we can create functions to look up fields
    print(company car ) -- ="Ford"

    print(Vector 3 5 8 `vplus` Vector 9 2 8  ) -- =Vector 12 7 16
    print(Vector 3 9 7 `vectMult` 10  ) -- =Vector 30 90 70
    print(Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  ) -- =74.0

    let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    --comparison
    print(mikeD == mikeD) -- =True
    --convert to string
    print("Mike D is : " ++show mikeD) -- ="Mike D is : Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
    --convert from string to Person data type
    print(read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person ) -- =Person {firstName = "Michael", lastName = "Diamond", age = 43}

    --can convert to string as it is part of the show typeclass
    print(show Wednesday) -- ="Wednesday"

    --compare because it is part of the Ord typeclass
    print(Saturday > Friday) -- =True

    --As it is Bounded, get the lowest and highest day
    print(minBound :: Day) -- =Monday
    print(maxBound :: Day) -- =Sunday

    --successor and predecessor
    print(succ Monday) -- =Tuesday
    print(pred Tuesday) -- =Monday  

