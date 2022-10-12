--Task 1
inc :: Int -> Int
inc x | x >= 0     = x + 1
      | otherwise  = error "Arg must be positive!"

dec :: Int -> Int
dec x | x > 0     = x - 1
      | otherwise  = error "Arg must be positive!"

pls :: Int -> Int -> Int
pls x y | y > 0     = pls (inc x) (dec y)
        | otherwise = x

mns :: Int -> Int -> Int
mns x y | y > 0     = mns (dec x) (dec y)
        | otherwise = x

mlt :: Int -> Int -> Int
mlt x 0 = 0
mlt 0 x = 0
mlt x y = mltHelper x x (dec y)
    where mltHelper :: Int -> Int -> Int -> Int
          mltHelper x t n | n > 0      = mltHelper x (pls x t) (dec n)
                          | otherwise  = t

--Task 2

myMax :: Int -> Int -> Int
myMax x y | x > y     = x
          | otherwise = y

myMin :: Int -> Int -> Int
myMin x y | x < y     = x
          | otherwise = y

--Task 3

myDiv :: Int -> Int -> Int
myDiv x y | (x < 0) || (y < 0) = error "Args have to be positive"
          | otherwise          = divHelp x y 0
    where divHelp :: Int -> Int -> Int -> Int
          divHelp x y n | (mlt y n) >= x  = dec n
                        | otherwise       = divHelp x y (inc n)


--Task 4

myDiv1 :: Int -> Int -> Int
myDiv1 x y | (x < 0) || (y < 0) = error "Args have to be positive"
           | otherwise          = divHelp x y 0
    where divHelp :: Int -> Int -> Int -> Int
          divHelp x y n | x > 0     = divHelp (x - y) y (inc n)
                        | x == 0    = n
                        | x < 0     = dec n

--Task 5

myMod :: Int -> Int -> Int
myMod x y | (x < 0) || (y < 0) = error "Args have to be positive"
          | otherwise          = x - (mlt (myDiv x y) y)

myMod1 :: Int -> Int -> Int
myMod1 x y | (x < 0) || (y < 0) = error "Args have to be positive"
           | otherwise          = x - (mlt (myDiv1 x y) y)


--Task 6

modEq0 :: Int -> Int -> Bool
modEq0 x y | (x < 0) || (y < 0) = error "Args have to be positive"
           | otherwise          = helper x y
           where helper :: Int -> Int -> Bool
                 helper x y | x == 0     = True
                            | x > 0      = helper (x - y) y
                            | x < 0      = False

--Task 7
nd :: Int -> Int
nd x | (x < 0)    = error "Args have to be positive"
     | otherwise  = length [d | d <- [1..x], myMod x d == 0]

--Task 8
sumd :: Int -> Int
sumd x | (x < 0)    = error "Args have to be positive"
       | otherwise  = sum [d | d <- [1..x], myMod x d == 0]

--Task 9
primeCheck :: Int -> Bool
primeCheck x | (x < 0)    = error "Args have to be positive"
             | otherwise  = nd x == 2

--Task 10
pnd :: Int -> Int
pnd x | (x < 0)    = error "Args have to be positive"
      | otherwise  = length [d | d <- [1..x], primeCheck d == True]


--Task 11
nod :: Int -> Int -> Int
nod x y | (x < 0) || (y < 0)    = error "Args have to be positive"
        | otherwise             = last [d | d <- [1..(myMin x y)], myMod x d == 0 && myMod y d == 0]

--Task 12
nok :: Int -> Int -> Int
nok x y | (x < 0) || (y < 0)    = error "Args have to be positive"
        | otherwise             = seeker x y (myMax x y)
        where seeker :: Int -> Int -> Int -> Int
              seeker x y n | myMod n x == 0  && myMod n y == 0    = n
                           | otherwise                            = seeker x y (inc n)


--Task 13
mg (g) (x) = head [y | y <- [1..], isDefined g x y]
    where isDefined g x y = (all (\t -> g(t, x) /= 0) [1..(y - 1)]) && (g(y, x) == 0)

--Task 14

mr (r) (x) = head [y | y <- [1..], isDefined r x y]
    where isDefined r x y = (all (\t -> r(t, x) == False) [1..(y - 1)]) && (r(y, x) == True)

--Task 15

mySqrt x = head [y | y <- [1..], isDefined x y]
    where isDefined x y = (all (\t ->  (x < (t + 1)^2) == False ) [1..(y - 1)]) && ((x < (y + 1)^2) == True)

--Task 16

myDivide x1 x2 = head [y | y <- [1..], isDefined x1 x2 y]
    where isDefined x1 x2 y = ((all (\t ->  (x1 < (t + 1) * x2) == False ) [1..(y - 1)]) && (x1 < (y + 1) * x2) == True)






