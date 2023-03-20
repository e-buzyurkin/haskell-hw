import Control.Monad.State
import Data.List
-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

check :: IO ()
check = do
    let resultState = solveState exampleProgram
    if exampleAns `listEq` resultState
        then putStrLn "OK!"
        else error "something wrong:("
    where listEq l r = leftInRight && rightInLeft
            where leftInRight = all (\x -> x `elem` r) l
                  rightInLeft = all (\x -> x `elem` l) r

data Value = Literal String | VariableReference String
    deriving (Show)
data Command = Command { varName :: String, whatToPut :: Value }
    deriving (Show)

-- you can choose something else!
type InterpreterState = [(String, String)]

solveState :: String -> [(String, String)]
solveState input = interpretToState (map parse $ lines input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")

--скопировал из документации
split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parse :: String -> Command
parse s = do
    let parts = split '=' s
    let var = head parts
    let val_str = head $ tail parts
    let value = if ((head val_str) == '$') then 
            (VariableReference (tail $ val_str))
        else
            (Literal val_str)
    (Command var value)

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
interpretMany :: [Command] -> State InterpreterState ()
interpretMany [] = return ()
interpretMany (x:xs) = do
    interpretOne x
    interpretMany xs

valueOfFunc :: String -> InterpreterState -> String
valueOfFunc searchedVar ((var, val) : state) | var == searchedVar   = val
                                             | otherwise            = valueOfFunc searchedVar state 

getCommand :: Command -> Value -> InterpreterState -> (String, String)
getCommand cmd (Literal val) curState = (varName cmd, val)
getCommand cmd (VariableReference value) curState = (varName cmd, valueOfFunc value curState)

-- using get, set and other State functions, interpret the command
interpretOne :: Command -> State InterpreterState ()
interpretOne cmd = do
    curState <- get

    let value = whatToPut cmd

    let res = getCommand cmd value curState

    put ([st | st <- curState, fst st /= varName cmd] ++ [res])


-- you can choose other type for result
interpretToState :: [Command] -> [(String, String)]
interpretToState commands = execState (interpretMany commands) emptyState
    where emptyState = []