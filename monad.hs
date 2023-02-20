type Variables = (Int, Int, Int)
data MonadVars a where
  MonadVars :: {actualComputation :: (Variables -> (a, Variables))}
                 -> MonadVars a

runComputation :: MonadVars a -> Variables -> (a, Variables)
runComputation computation vars = actualComputation computation $ vars

getVars :: MonadVars Variables
getVars = MonadVars $ \vars -> (vars, vars)

putVars :: Variables -> MonadVars ()
putVars newVars = MonadVars $ \vars -> ((), newVars)

instance Functor MonadVars where
    -- fmap :: (a -> b) -> (MonadVars a) -> (MonadVars b)
    fmap :: (a -> b) -> MonadVars a -> MonadVars b
    fmap f (MonadVars comp) =
        MonadVars $ \vars -> (f (fst $ comp vars), snd $ comp vars)

instance Applicative MonadVars where
    pure x = MonadVars $ \vars -> (x, vars)
    (<*>) :: MonadVars (a -> b) -> MonadVars a -> MonadVars b
    (MonadVars compF) <*> (MonadVars compX) =
        MonadVars $ \vars -> ((fst $ compF vars) (fst $ compX $ snd $ compF vars), (snd $ compX $ snd $ compF vars))
        -- то, что написано выше, совершенно нечитабельно, я попытался немного прояснить ниже
        -- если я пытаюсь сделать то, что ниже, через where, то с меня требуют vars, которые я не знаю как передать
        -- (a_to_b a_vars, out_vars)       
        -- a_to_b = fst $ compF vars
        -- temp_vars = snd $ compF vars
        -- a_vars = fst $ compX temp_vars
        -- out_vars = snd $ compX temp_vars


instance Monad MonadVars where
    (>>=) :: MonadVars a -> (a -> MonadVars b) -> MonadVars b
    (MonadVars comp1) >>= comp2 = MonadVars $ \vars -> runComputation (comp2 (fst $ comp1 vars)) (snd $ comp1 vars)
 


computation :: MonadVars Int
computation = do
    (x1, x2, x3) <- getVars
    putVars (x1 + 2, x2 + 1, x3 * 3)
    return 200

main :: IO ()
main = do
    print $ runComputation computation (20, 30, 40)