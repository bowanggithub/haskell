import qualified Data.Map as Map
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
--data Person = Person { firstName :: String
--                     , lastName :: String
--                    , age :: Int
--                     , height :: Float
--                     , phoneNumber :: String
--                     , flavor :: String
--                     } deriving (Show)

--data Car = Car String String Int deriving (Show)
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq, Ord, Show, Read, Bounded, Enum)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]

--data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

--class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ =False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
