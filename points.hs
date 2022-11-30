listPoints :: [(Int, Int)] -> [(Int, Int)]
listPoints ((10, 10):l) = (10, 10) : l
listPoints (( x, 10):l) = listPoints ((x + 1, 0) : (x, 10) : l)
listPoints (( x,  y):l) = listPoints ((x, y + 1) : (x, y)  : l)

data Point = Point Int Int deriving (Eq,Show)

listPoints' :: [Point] -> [Point]
listPoints' ((Point 10 10):l) = Point 10 10 : l
listPoints' ((Point  x 10):l) = listPoints' (Point (x + 1) 0 : Point x 10 : l)
listPoints' ((Point  x  y):l) = listPoints' (Point x (y + 1) : Point x  y : l)

-- Make your thing with tuples above work with this datastructure below.

data Point3D = Point3D Int Int Int deriving (Eq, Show)

listPoints3D :: [Point3D] -> [Point3D]
listPoints3D ((Point3D 10 10 10):l) = Point3D 10 10 10 : l
listPoints3D ((Point3D  x 10 10):l) = listPoints3D (Point3D (x + 1) 0 0 : Point3D x 10 10 : l)
listPoints3D ((Point3D  x  y 10):l) = listPoints3D (Point3D x (y + 1) 0 : Point3D x y 10 : l)
listPoints3D ((Point3D  x  y  z):l) = listPoints3D (Point3D x y (z + 1) : Point3D x y z : l)


main = do
          print . reverse $ listPoints [(0, 0)]
          print . reverse $ listPoints' [Point 0 0]
          print . reverse $ listPoints3D [(Point3D 0 0 0)] 
          
          
          
          
{-
listPoints'' :: [Point3D] -> [Point3D]
listPoints'' ((Point3D 10 10 10):l) = Point3D 10 10 10 : l
listPoints'' ((Point3D  x 10 10):l) = listPoints'' (Point3D (x + 1) 0 : Point3D x 10 : l)
listPoints'' ((Point3D  x  y z):l) = listPoints'' (Point3D x (y + 1) : Point3D x  y : l)
-}