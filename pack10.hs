import Data.Char
import Barans
import Data.Maybe
-- example:
-- makeArt 0 == ""
-- makeArt 1
-- wil result in:
-- a
-- makeArt 3
{-
**a**
*b*b*
c***c
*b*b*
**a**
-}
makeArt :: Int -> String
makeArt 0 = ""
makeArt n = foldl (\s i -> s ++ (constructLine i) ++ "\n") "" [1..(2 * n - 1)]
    where constructLine :: Int -> String
          constructLine i = [if (x == n - (i - (2 * (i - n)) * (fromEnum (i > n)) - 1) 
                                || x == n + (i - (2 * (i - n)) * (fromEnum (i > n)) - 1))
                             then chr(ord('a') - 1 + i - (2 * (i - n)) * (fromEnum (i > n))) else '*' 

                                            | x <- [1..(2 * n - 1)]]

--i - (2 * (i - n)) * (fromEnum (i > n))

    -- let len = 2 * n - 1
    -- let constructLine l = [if x == n - (i - 1) || x == n + (i - 1) 
    --                         then chr(ord('a') + (i - 1)) else '*' | x <- [1..n]]
    -- let constructResult m = foldl (\i s -> s ++ constructLine i ++ '\n') "" [1..m]
    -- constructResult n

{-
1. Найти дедушку -- отца матери
2. Найти прадеда (отца найденного выше)
3. Найти список всех родителей и список бабушек с дедушками 
    для данного барашка (т.е. две функции)
4. Является ли он сиротой (по базе данных)?
Пусть есть список селекционных барашков:
selected_barans = ["i3", "i5", "i6", "i9", "i12"]
5. Нужно написать функцию, которая для данного барашка
     возвращает имя селекционного папы, 
    обёрнутого конструктором Just, и Nothing, 
    если такового нет (например Just "i6").
6. Нужно написать функция, которая находит 
селекционного ближайшего предка 
    по мужской линии 
    (либо, если такового нет, возвращает Nothing).
-}
grandFather :: Sheep -> Maybe Sheep
grandFather s | mother s == Nothing   = Nothing
              | otherwise             = father $ fromJust $ mother s

greatGrandFather s | grandFather s == Nothing   = Nothing
                   | otherwise                  = father $ fromJust $ grandFather s

listOfParents s 
        | mother s == Nothing && father s == Nothing = []
        | mother s == Nothing                    = [fromJust $ father s]
        | father s == Nothing                = [fromJust $ mother s]
        | otherwise                      = [fromJust $ mother s] ++ [fromJust $ father s]


listOfGrandParents s = foldl (\list s -> list ++ (listOfParents s)) [] (listOfParents s)

isParentless s = isit (listOfParents s)
    where isit [] = True
          isit _  = False

selected_barans :: [Sheep]
selected_barans = ["i3", "i5", "i6", "i9", "i12"]

selectedFather s | father s == Nothing                          = Nothing
                 | elem (fromJust $ father s) selected_barans == False     = Nothing
                 | otherwise                            = father s

closestFathersRelative s | father s == Nothing    = Nothing
                         | elem (fromJust $ father s) selected_barans == True  = father s
                         | otherwise        = closestFathersRelative $ fromJust $ father s

