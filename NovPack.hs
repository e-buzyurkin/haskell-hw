import Data.List

refer_to_index :: [a] -> Int -> a
refer_to_index list i = go_through list 0
    where go_through (x:xs) n | n == i          = x
                              | otherwise       = go_through xs (n + 1)

init1 :: [a] -> [a]
init1 [] = []
init1 list = fst $ foldl (\(acc, i) x -> if i + 1 == length list then (acc, i + 1) else (acc ++ [x], i + 1)) ([], 0) list

concatLists :: [a] -> [a] -> [a]
concatLists a b = foldr (\x acc -> x:acc) b a

cycle1 :: [a] -> [a]
cycle1 list = list ++ cycle1 list

take1 :: Int -> [a] -> [a]
take1 n list = takeHelp n list []
    where takeHelp 0 _ acc = acc
          takeHelp n (x:xs) acc = takeHelp (n - 1) xs (acc ++ [x])

inits :: [a] -> [[a]]
inits [] = []
inits list = initsHelp list [] (length list)
    where initsHelp _ acc (-1) = acc
          initsHelp list acc n = initsHelp (init list) (list:acc) (n - 1)

tails :: [a] -> [[a]]
tails [] = []
tails list = tailsHelp list [] (length list)
    where tailsHelp _ acc (-1) = acc
          tailsHelp list acc n = tailsHelp (tail list) (acc ++ [list]) (n - 1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 x list = foldl (\flag h -> if (h == x && flag == False) then True else flag == True) False list

nub :: Eq a => [a] -> [a]
nub list = foldl (\acc x -> if elem x acc then acc else (acc ++ [x])) [] list

updElmBy :: [a] -> Int -> a -> [a]
updElmBy list n elem = fst $ foldl (\(acc, i) x -> if (i == n) then ((acc ++ [elem]), i + 1) else (acc ++ [x], i + 1)) ([], 0) list

swp :: [a] -> Int -> Int -> [a]
swp list i j = updElmBy (updElmBy list j (list !! i) ) i (list !! j)

--sorry za translit, inache comments v kashu slivayutsa

--picking delaet primerno tak: [1, 2, 3] -> [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
--tak mi delaem kazhdiy element pervim, i k nemu perebiraem vse vozmozhnie hvosti
permutations1 :: [a] -> [[a]]
permutations1 [] = [[]]
permutations1 xs = [picked_head : permed_tail | (picked_head, rem_tail) <- picking xs, 
                                                permed_tail <- permutations1 rem_tail]
    where picking []     = []
          picking (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picking xs]


--na kazhdiy element kladem v finalniy spisok ego samogo,
--kopiyu tekushego finalnogo spiska, no s golovoy v vide
--nashego tekushego elementa 
--3 [[1], [1, 2]] -> [[3], [3, 1], [3, 1, 2]]
subsequences1 :: [a] -> [[a]]
subsequences1 list = [[]] ++ foldr (\x s -> [x] : addHead x s ++ s) [] list
    where addHead x list = foldl (\acc tl -> acc ++ [x:tl]) [] list



cubsumR :: [Int] -> Int
cubsumR list = foldr (\s x -> x + s^3) 0 list

cubsumL :: [Int] -> Int
cubsumL list = foldl (\s x -> s + x^3) 0 list

fact :: Int -> Int
fact x = foldl (\n x -> n * x) 1 [2..x]

expT :: Double -> Int -> Double
expT x n = foldl (\e n -> e + (x ^ n) / (fromIntegral $ fact n)) 1 [1..n]

howmany :: Eq a => a -> [a] -> Int
howmany x list = foldl (\n elem -> if elem == x then n + 1 else n) 0 list

good_letters = "aeiou"
bad_letters = "tnrsh"

howmany_g_b_letters :: [Char] -> (Int, Int)
howmany_g_b_letters str = foldl (\(g, b) c -> if elem c good_letters then (g + 1, b)
                                              else if elem c bad_letters then (g, b + 1)
                                              else (g, b)) (0, 0) str

intersperse :: a -> [a] -> [a]
intersperse ins list = foldl (\acc x -> (acc ++ [ins] ++ [x])) [head list] (tail list)

cycleshift (x:xs) = xs ++ [x]

rotateL :: [a] -> [[a]]
rotateL list = fst $ foldl (\(acclist, curlist) _ -> (acclist ++ [cycleshift curlist], cycleshift curlist)) ([], list) [1..(length list)]

rotateR :: [a] -> [[a]]
rotateR list = fst $ foldr (\_ (acclist, curlist) -> (acclist ++ [cycleshift curlist], cycleshift curlist)) ([], list) [1..(length list)]






