module Lib where


-----------------------------
--IMPORTS
-----------------------------
import Data.List
import Data.Function
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------
--DATATYPES
-----------------------------

-- | Fruit datatype, each fruit has a height, weight, color score, and a label (the fruit species)
data Fruit = Fruit {height :: Double, weight :: Double, color :: Double, label :: String}

-----------------------------
--VARIABLES
-----------------------------

test :: Fruit
test = Fruit 10 40 1 "Unknown"

orange1 :: Fruit
orange1 = Fruit 5 25 7 "Orange"

orange2 :: Fruit
orange2 = Fruit 5 27 7 "Orange"

banana1 :: Fruit
banana1 = Fruit  15 50 5 "Banana"

banana2 :: Fruit
banana2 = Fruit  17 60 5 "Banana"

apple1 :: Fruit
apple1 = Fruit  10 50 10 "Apple"

apple2 :: Fruit
apple2 = Fruit  11 55 9 "Apple"

apple3 :: Fruit
apple3 = Fruit  11 50 1 "Apple"

-- | List of fruits
fruits :: [Fruit]
fruits = [orange1, orange2, banana1, banana2, apple1, apple2, apple3] 

------------------------------
--FUNCTIONS
------------------------------

-- | Old function, now replaced with map (fst) 
setToArr :: [(String, Double)] -> [String]
setToArr [] = []
setToArr (x:xs) = fst x : setToArr xs

-- | Finds the highest occurring value in a list
highestOccurring :: Ord a => [a] -> a --Takes a list of a's, returns a value
highestOccurring items = head $ minimumBy (flip $ comparing length) . group . sort $ items

-- | Sorts a list of tuples, sorted on b's
mySort :: Ord b => [(a, b)] -> [(a, b)] --takes a list of tuples, returns a list of tuples (sorted on b)
mySort = sortBy (compare `on` snd)

-- | Old function for distance 
calcDistance :: Fruit -> Fruit -> Double
calcDistance f1 f2 = sqrt(((height f1 - height f2)**2) + ((weight f1 - weight f2)**2) + ((color f1 - color f2)**2))

-- | New, more general function for calculating the distance
calcDistance' :: [Double] -> [Double] -> Double --takes two lists of doubles and returns a double
calcDistance' x y = sqrt(sum(map(**2) (zipWith (-) x y)))

-- | Makes a list of tuples containing a string and a double
distanceArr :: [Fruit] -> Fruit -> [(String, Double)] --takes a list of fruits and a fruit
distanceArr [] _ = [] 
distanceArr (x:xs) f = (label x, calcDistance' [height x, weight x, color x] [height f, weight f, color f]) : distanceArr xs f

-- | Calculates the k (k = num) neighbors of a fruit, and determines what species the given fruit is
kNearest :: Int -> Fruit -> String --takes an int and a fruit, returns a string (fruit species)
kNearest k f = highestOccurring $ map fst $ take k $ mySort $ distanceArr fruits f
