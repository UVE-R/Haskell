--create a new data type
data TrafficLight = Red | Yellow | Green  

--make an instance of Eq
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

--make an instance of Show 
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

main = do
    
    print(Red == Red) -- =True
    print(Red == Yellow) -- =False

    print([Red,Yellow,Green]) -- =[Red light,Yellow light,Green light]

    
