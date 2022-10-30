import Data.List ( nub )
import Data.Char (digitToInt, isDigit)
import qualified Data.Map as Map
import Geometry

doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) -1
numbers = [1,2,3,4] !! 1
boomBangs xs = [if x < 10 then "BooM" else "Bang" | x <- xs, odd x]

bmiTell :: Double -> Double -> String
bmiTell weight height 
    | bmi <= skinny = "You're light " ++ show bmi
    | bmi <= normal = "You're middle " ++ show bmi
    | otherwise = "You're fat " ++ show bmi
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0

replicate' :: Int -> a -> [a]
replicate' i j
    | i <= 0 = []
    | otherwise = j : replicate' (i - 1) j

take' :: Int -> [a] -> [a]
take' i arr
    | i <= 0 = []
take' _ [] = []
take' i (x:xs) = x : take' (i - 1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) =
    let smallerOrEq = [x | x <- xs, x <= p]
        larger = [x | x <- xs, p < x]
    in quicksort smallerOrEq ++ [p] ++ quicksort larger

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n = n : collatz (n * 3 + 1)

collatzCount :: Int
collatzCount = length(filter isLong (map collatz [1..100]))
    where isLong xs = 15 < length xs
flip' :: (a->b->c) -> b -> a -> c
flip' = \f -> \x -> \y -> f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

maximum' :: (Ord a) => [a] -> a
-- maximum' = foldl1 (\acc x -> if acc < x then x else acc)
maximum' = foldl1 max

sqrtSums :: Int
sqrtSums = length(takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBook :: [(String, String)]
phoneBook =
    [
        ("betty", "555-0192"),
        ("betty", "777-9019"),
        ("rinel", "717-1119"),
        ("mod", "343-5439"),
        ("mod", "754-5329"),
        ("betty", "2317-9019"),
        ("rinel", "777-9019"),
        ("mic", "32147-9019")
    ]

phoneBookToMap :: Ord k => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith add
    where add number1 number2 = number1 ++ ", " ++ number2

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Int,
    phoneNumber :: String,
    flavor :: String
} deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- 型シノニム
type PhoneNumber = String
type Age = String