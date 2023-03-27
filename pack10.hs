import Data.Char
import Barans
import Data.Maybe
import Control.Monad (guard)
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
grandFather s = mother s >>= father


greatGrandFather s = grandFather s >>= father

listOfParents s = (maybeToList $ mother s) ++ (maybeToList $ father s)


listOfGrandParents s = (map listOfParents) . listOfParents

isParentless = null . listOfParents

selected_barans :: [Sheep]
selected_barans = ["i3", "i5", "i6", "i9", "i12"]



selectedFather s = do
    case father s of
        Nothing -> Nothing
        Just f -> if elem f selected_barans then Just f else Nothing


closestFathersRelative s = if selectedFather s /= Nothing then selectedFather s else do
    case father s of 
        Nothing -> Nothing
        Just f -> closestFathersRelative f 
    
    case mother s of
        Nothing -> Nothing
        Just m -> closestFathersRelative m
    
    Nothing