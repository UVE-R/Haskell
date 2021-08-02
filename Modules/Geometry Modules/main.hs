import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube


main = do 
    --sphere area and volume
    print(Sphere.area 5) -- =314.15927
    print(Sphere.volume 3) -- =113.097336
    

    --cuboid area and volume
    print(Cuboid.area 10 5 4) -- =220.0
    print(Cuboid.volume 10 5 4) -- =200.0
     

    --cube area and volume
    print(Cube.area 10) -- =600.0
    print(Cube.volume 4) -- =64.0
    
    
