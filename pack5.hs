import Data.Char
import Data.Maybe
-- pack 5 ' maybe, recursion, structs '

-- task 1 ' quadratic equation '
-- Solve quadratic equation
-- In case it has no roots, return Nothing
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c | disc a b c < 0   = Nothing
                      | otherwise        = Just $ (( (-b + sqrt(disc a b c)) / (2 * a) ), (-b - sqrt(disc a b c)) / (2 * a))
                            where disc a b c = (b^2 - 4 * a * c)

-- task 2 ' maybe lists stdlib '
-- Implement the following lists functions using Maybe data structure

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead n  = Just $ head n

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail n = Just $ tail n

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit n  = Just $ init n

-- finds first element x in lst, such that (predicate x == True)
-- if no such element exists, returns Nothing
maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind predicate [] = Nothing
maybeFind predicate (x:xs) | predicate x == True    = Just $ x
                           | otherwise              = maybeFind predicate xs

-- task 3 ' pattern matching with data structures '
-- implement undefined functions

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- dogs which are good boys
goodBoys :: [Dog]
goodBoys = [dog | dog <- dogs, isGoodBoy dog]

-- dogs with name longer than 7 symbols
longNamedDogs :: [Dog]
longNamedDogs = [dog | dog <- dogs, (length (name dog)) > 7]

-- among dogs, which is the most popular gender?
mostPopularDogGender :: Gender
mostPopularDogGender | (foldl (\n dog -> if (gender dog == Male) then n + 1 else n) 0 dogs) >  div (length dogs) 2 = Male
                     | otherwise                                                                                   = Female

oldestDog :: Dog
oldestDog = foldl (\maxdog curdog -> if (age curdog) > (age maxdog) then curdog else maxdog) (head dogs) dogs

averageDogAge :: Double
averageDogAge = summary / len
    where summary = fromIntegral (foldl (\s dog -> s + (age dog)) 0 dogs)
          len = fromIntegral (length dogs)

-- finds dogs with given breed
dogsByBreed :: DogBreed -> [Dog]
dogsByBreed givenBreed = [dog | dog <- dogs, (breed dog) == givenBreed]

----- data structures
-- task 4.1

-- Создайте тип, который реализует комплексные числа
-- создайте функции, которые реализуют:
-- - сумму, разницу
-- - умножение, деление
-- - взятие сопряженного
-- - взятие абсолютного значения


data Complex = Complex { real :: Double ,
                         imaginary :: Double
                       } deriving (Show)

sumComplex :: Complex -> Complex -> Complex
sumComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
--sumComplex (Complex 1 2) (Complex 6 3)
-- (7 + 5i)

subComplex :: Complex -> Complex -> Complex
subComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)
--subComplex (Complex 1 2) (Complex 6 3)
-- (-5 - i)

multComplex :: Complex -> Complex -> Complex
multComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
--multComplex (Complex 1 2) (Complex 6 3)
-- (0 + 15i)

divComplex :: Complex -> Complex -> Complex
divComplex (Complex r1 i1) (Complex r2 i2) = Complex ( (r1 * r2 + i1 * i2) / (r2^2 + i2^2) ) ( (r2 * i1 - r1 * i2) / (r2^2 + i2^2) )
--divComplex (Complex 1 2) (Complex 6 3)
-- (0.26666666 + 0.2i)

conjugateComplex :: Complex -> Complex
conjugateComplex (Complex r i) = Complex (r) (-1 * i)
--conjugateComplex (Complex 1 2)
-- (1 - 2i)

absComplex :: Complex -> Double
absComplex (Complex r i) = sqrt(r^2 + i^2)
--absComplex (Complex 6 3)
-- (6.7082039...)

-- Создайте тип, который образует односвязный список (<=> список имеет голову и хвост, либо является пустым)
-- реализуйте  для него следующие методы:

--вообще не понимаю че происходит
--UPD: doshlo
data MyList a = EmptyList | MyList a (MyList a)
    deriving (Show)

fromList :: [a] -> MyList a
fromList [] = EmptyList
fromList n = MyList (head n) (fromList $ tail n)

toList :: MyList a -> [a]
toList EmptyList = []
toList (MyList h t) = (h:(toList t))

reverseMyList :: MyList a -> MyList a
reverseMyList EmptyList = EmptyList
--reverseMyList n = fromList $ reverse $ toList n -- не думаю, что такое решение подойдет
reverseMyList n = revHelper n EmptyList
    where revHelper EmptyList acc = acc
          revHelper (MyList h t) acc = revHelper t (MyList h acc)

-- should do the same thing as standard map
mapMyList :: (a -> b) -> MyList a -> MyList b
--mapMyList f list = fromList $ map f (toList list) -- аналогично
mapMyList f EmptyList = EmptyList
mapMyList f (MyList h t) = MyList (f h) (mapMyList f t)
