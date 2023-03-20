import Prelude hiding (head, tail, maximum)
import Data.Maybe
import Control.Monad.State
import System.Random

type GreekData = [(String, [Int])]
greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay a b = Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldl max x (x:xs)

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay [a] = Just []
tailMay (x:xs) = Just xs

{-
 tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the maximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert 
 your two Integers to Doubles for the final call to divMay.
-}

newList :: GreekData -> String -> [Int] -> [Int]
newList [] str acc = acc
newList (x:xs) str acc | fst x == str   = newList xs str (acc ++ (snd x))
                       | otherwise      = newList xs str acc

queryGreek :: GreekData -> String -> Maybe Double
queryGreek list str = 
        divMay (fromIntegral $ fromJust $ maximumMay $ newList list str [])
                 (fromIntegral $ fromJust $ headMay $ newList list str [])


-- queryGreek greekDataA "alpha" == Just 2.0

-- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro list str = do
    let xs = newList list str []
    let max_int = maximumMay xs
    divMay (fromIntegral $ fromJust $ max_int) (fromIntegral $ fromJust $ headMay xs)


-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus = undefined


-- state monad
type RandState = Int

a = 103421
b = -93572334

rollDice :: State RandState Int
rollDice = do
    seed <- get
    let temp = mod (a * a - seed * b + 4634 * a - b * 4253422 + 3534 * a * seed + b) 6
    put temp
    return temp


game :: State RandState String
game = do
    firstPlayerRes <- rollDice
    secondPlayerRes <- rollDice
    if firstPlayerRes > secondPlayerRes then 
        return "First wins" 
    else
        return "Second wins"

runGame :: String
runGame = evalState game startSeed
    where startSeed = 3