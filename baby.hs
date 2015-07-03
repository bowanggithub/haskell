doubleMe x = x+x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
		      then x
		      else x*2
doubleSmallNUmber' x= (if x >100 then x else x*2)+1
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [if x<10 then "BOOM!" else "BANG!" | x<- xs, odd x]
--length' xs = sum[1| _ <- xs]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c| c<- st, c `elem` ['A'..'Z']]
--factorial :: Integer -> Integer
--factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2* pi * r
circumference' ::Double->Double
circumference' r = 2 *pi * r

lucky :: (Integral a) => a-> String
lucky 7 = "Lucky Number Seven!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a-> a
factorial 0 =1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "whatever"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
--addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: "
			++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] =0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
