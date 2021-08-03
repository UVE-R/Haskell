--deriving (Show) allows the data type to be printed
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  


--return the surface of a shape (area)
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

--shift the shape in the x and y
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a)  (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


main = do

    print(surface (Rectangle (Point 0 0) (Point 100 100))) -- =10000.0  

    print(surface (Circle (Point 0 0) 24)) -- =1809.5574  

    print(Circle (Point 10 10) 10) -- =Circle (Point 10.0 10.0) 10.0

    print(nudge (Circle (Point 34 34) 10) 5 10) -- =Circle (Point 39.0 44.0) 10.0

    print(nudge (Rectangle (Point 0 0) (Point 100 100)) 10 20) -- =Rectangle (Point 10.0 20.0) (Point 110.0 120.0)

    
