data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
--surface :: Shape -> Float
--surface (Circle _ _ r) =pi * r^2
--surface (Rectangle x1 y1 x2 y2) = (abs $ x2-x1)*(abs $ y2-y1)
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2-x1)*(abs $ y2-y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

--data Person = Person String String Int Float String String deriving (Show)
--firstName :: Person -> String  
--firstName (Person firstname _ _ _ _ _) = firstname  
--lastName :: Person -> String  
--lastName (Person _ lastname _ _ _ _) = lastname    
--age :: Person -> Int  
--age (Person _ _ age _ _ _) = age  
--height :: Person -> Float  
--height (Person _ _ _ height _ _) = height  
--phoneNumber :: Person -> String  
--phoneNumber (Person _ _ _ _ number _) = number  
--flavor :: Person -> String  
--flavor (Person _ _ _ _ _ flavor) = flavor  
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

--data Car = Car String String Int deriving (Show)
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
