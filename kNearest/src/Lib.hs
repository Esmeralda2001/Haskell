module Lib where



import Data.List
import Data.Function
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map


-- * DATATYPES

-- | Fruit datatype, each fruit has a height, weight, color, and a label (the fruit species)
data Fruit = Fruit {height :: Double, weight :: Double, color :: Colour, label :: Species}

-- | Colour datatype, consists of the following possible values: Green, Yellow, Red, Orange
data Colour = Green | Yellow | Orange | Red deriving (Enum, Show)

-- | Species datatype, consists of the following possible values: Apple, Mandarin, Banana, Unknown
data Species = Apple | Mandarin | Banana | Unknown deriving (Show, Ord, Eq)


-- * VARIABLES

test :: Fruit
test = Fruit 10 40 Green Unknown

orange1 :: Fruit
orange1 = Fruit 5 25 Orange Mandarin

orange2 :: Fruit
orange2 = Fruit 5 27 Orange Mandarin

banana1 :: Fruit
banana1 = Fruit  15 50 Yellow Banana

banana2 :: Fruit
banana2 = Fruit  17 60 Yellow Banana

apple1 :: Fruit
apple1 = Fruit  10 50 Red Apple

apple2 :: Fruit
apple2 = Fruit  11 55 Red Apple

apple3 :: Fruit
apple3 = Fruit  11 50 Green Apple


-- | List of fruits
fruits :: [Fruit]
fruits = [orange1, orange2, banana1, banana2, apple1, apple2, apple3]

 

-- * FUNCTIONS

-- | Old function, now replaced with map (fst) 
setToArr :: [(String, Double)] -> [String]
setToArr [] = []
setToArr (x:xs) = fst x : setToArr xs

-- | Finds the highest occurring value in a list
highestOccurring :: Ord a => [a] -> a
highestOccurring = head . maximumBy (comparing length) . group . sort

-- | Sorts a list of tuples, sorted on b's
mySort :: Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (comparing snd)

-- | Old function for distance (commented like this, because the function will otherwise cause errors
-- | calcDistance :: Fruit -> Fruit -> Double
-- | calcDistance f1 f2 = sqrt(((height f1 - height f2)**2) + ((weight f1 - weight f2)**2) + ((color f1 - color f2)**2))

-- | New, more general function for calculating the distance
calcDistance' :: [Double] -> [Double] -> Double
calcDistance' = ((sqrt . sum . map (** 2)) .) . zipWith (-)

-- | Makes a list of tuples containing a Species and a Double
distanceArr :: [Fruit] -> Fruit -> [(Species, Double)]
distanceArr [] _ = [] 
distanceArr (x:xs) f = (label x, calcDistance' [height x, weight x, fromIntegral(fromEnum (color x))] [height f, weight f, fromIntegral(fromEnum (color f))]) : distanceArr xs f

-- | Calculates the k (k = num) neighbors of a fruit, and determines what species the given fruit is
kNearest :: Int -> Fruit -> Species
kNearest k f = highestOccurring $ map fst $ take k $ mySort $ distanceArr fruits f
