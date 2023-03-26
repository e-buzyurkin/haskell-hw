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
       , Dog "Iacchus" 4 Female Beagle True
       , Dog "Socrates" 5 Male GoldenRetrievers True
       , Dog "Zeus" 5 Male Beagle True
       , Dog "Fiona" 4 Female Terrier True
       , Dog "Aisyah" 5 Female Beagle True] 



-- examples
-- how many dogs are of age 2, 4 and 6?
-- dogsAge246 :: Int
-- dogsAge246 = do
-- 	dogsAge2 <- dogsAge 2
-- 	dogsAge4 <- dogsAge 4
-- 	dogsAge6 <- dogsAge 6
-- 	return $ length $ dogsAge2 ++ dogsAge4 ++ dogsAge6


-- using do-notation, find such dogs, that they are male, 4-5 years old, not IrishSetter, good boys
-- and such dogs, that they are female, 4-5 years old, name is longer than 4 symbols
-- after finding those two groups, combine a list of all combinations they could be mated
combineCouples1 :: [a] -> [a] -> [(a, a)]
combineCouples1 [] _ = []
combineCouples1 _ [] = []
combineCouples1 (x:xs) (y:ys) = ((x, y) : (combineCouples1 xs ys))

combineCouples :: [a] -> [a] -> [Int] -> [[(a, a)]]
combineCouples _ _ [] = []
combineCouples (x:xs) (y:ys) (i:n) = [((x, y) : (combineCouples1 xs ys))] ++ combineCouples (xs ++ [x]) (y:ys) n

fact 1 = 1
fact n = n * fact (n - 1)

dogsQuery :: [[(Dog, Dog)]]
dogsQuery = do
       let males = filter (\(Dog name age gender breed isgb) -> (4 <= age && age <= 5) 
                                                                      && breed /= IrishSetter
                                                                      && gender == Male
                                                                      && isgb == True) dogs
       let females = filter (\(Dog name age gender breed isgb) -> (4 <= age && age <= 5) 
                                                               && gender == Female
                                                               && (length $ name) > 4) dogs
       
       combineCouples males females [1..fact (max (length males) (length females))]

printDogCouples :: [[(Dog, Dog)]] -> [[(String, String)]]
printDogCouples list = [[(name dog1, name dog2) | (dog1, dog2) <- sublist] | sublist <- list]

prettyBoard :: (Int, Int) -> (Int, Int) -> String
prettyBoard d1 d2 = concat $ [[if (mod j 2 == 1) then ' '
                               else symbol i j d1 d2 
                                                        | j <- [0..15]] ++ "\n" | i <- [0..7]]
       where symbol i j (x1, y1) (x2, y2) | i == x1 && (div j 2) == y1   = 'Q'
                                          | i == x2 && (div j 2) == y2   = 'q'
                                          | otherwise                    = '_'


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) | x1 == x2                         = True
                            | y1 == y2                         = True
                            | abs (x1 - x2) == abs (y1 - y2)   = True
                            | otherwise                        = False


-- let male45yoGoodBoys = filter (\(Dog name age gender breed isgb) -> (4 <= age && age <= 5) && breed /= IrishSetter && gender == Male && isgb == True) dogs