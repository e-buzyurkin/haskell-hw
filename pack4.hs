import Data.Char
--сделал два варианта printf, потому что не понял, "преобразованный в строку аргумент из args"..
--это весь список, преобразованный в строку, или голова списка, преобразованная в строку
printf :: String -> [Int] -> String
printf s args = before args s []
    where before :: [Int] -> String -> String -> String
          before [] [] acc = acc
          before args (c:cs) acc | c == '%' && (head cs == 's')     = after args (tail cs) acc
                                 | otherwise                        = before args cs (acc ++ [c])
                                 where after :: [Int] -> String -> String -> String
                                       after [] [] acc = acc
                                       after [] (c:cs) acc = after [] cs (acc ++ [c])
                                       after (x:xs) s acc = after xs s (acc ++ (show x))
--printf "iloveha%skell" [1, 2, 3] ----> "iloveha123kell"


printf1 :: String -> [Int] -> String
printf1 s args = before args s []
    where before :: [Int] -> String -> String -> String
          before args [] acc = acc
          --before [] [] acc = acc
          before (x:xs) (c:cs) acc | c == '%' && (head cs == 'd')     = before xs (tail cs) (acc ++ (show x))
                                   | otherwise                        = before (x:xs) cs (acc ++ [c])

--printf1 "iloveha%dkell" [5, 2, 3] ----> "iloveha5kell"


intToBinary :: Int -> String
intToBinary n = helper n []
    where helper :: Int -> String -> String
          helper 0 acc = acc
          helper n acc = helper (div n 2) ((show (mod n 2)) ++ acc)

--не понимаю как писать со свертками, поэтому не сделал 2 задачу
binToDec :: String -> Int
binToDec n = undefined

stringToInt :: String -> Int
stringToInt s = helper s ((length s) - 1) 0
    where helper :: String -> Int -> Int -> Int
          helper [] n acc = acc
          helper (c:cs) n acc = helper cs (n - 1) (acc + (ord c - 48) * 10^n)


findMissing :: [Int] -> Int
findMissing array = search (quicksort array)
    where search :: [Int] -> Int
          search [] = 0
          search (x:y:xs) | x + 1 == y       = search (y:xs)
                          | otherwise        = x + 1

          quicksort :: [Int] -> [Int]
          quicksort [] = []
          quicksort (x:xs) = (quicksort left) ++ [x] ++ (quicksort right)
              where left = filter (< x) xs
                    right = filter (>= x) xs
--сначала сортирую список, потом ищу недостающий
--сложность O(N(logN + 1))


bracketsCheck :: String -> Bool
bracketsCheck s = findFirst [] s
    where findFirst :: String -> String -> Bool
          findFirst [] "Unable To Find Second Bracket" = False
          findFirst [] [] = True
          findFirst left (c:right) | left == [] && c == ')'   = False
                                   | c == '('                 = findFirst [] (findSecond [] right)
                                   | otherwise                = findFirst (left ++ [c]) right
                                    where findSecond :: String -> String -> String
                                          findSecond left [] = "Unable To Find Second Bracket"
                                          findSecond left (c:right) | c == ')'   = (left ++ right)
                                                                    | otherwise  = findSecond (left ++ [c]) right

--решение заключается в том, что мы ищем открывающуюся скобку, потом парсим по тем скобкам, которые справа от нее
--из них ищем закрывающуюся скобку в пару к нашей открывающейся, если найдем, то возвращаем строку без нашей пары скобок
--если не найдем, то наша строка неправильная, соответственно возвращаем False
--если в нашей строке первым символом лежит закрывающаяся скобка, то выводим False
--если в строке остались только открывающиеся скобки, то возвращаем False

--т.к. списки в хаскелле односвязные, то придется парсить так называемым "переливанием": берем голову правой строки, если она
--нас не устраивает, то кладем ее в левую строку, если устраивает, то проводим нашу процедуру с правой строкой
--




