import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.Map as MP

-- fix this!!!
solveQ :: Double -> Double -> Double -> Maybe (Double, Double) 
solveQ (1.0) b c = Nothing
solveQ a b c = Just (2.0, 2.0)

-- fix this code.
solveUserQ :: MaybeT IO (Double, Double)
solveUserQ = do
  input <- lift getLine -- (1,2,3)
  let (a, b, c) = read input :: (Double, Double, Double)
  r <- _wtf $ solveQ a b c
  return r

 ---


type Log = [String]
data UserInfo { address :: String, name :: String, salary :: Int }

makeSureUserIsComfortableGivingInformation :: String -> MaybeT IO ()
makeSureUserIsComfortableGivingInformation infoName = undefined
-- print "Are you ok sharing " ++ infoName
-- get input from user
-- if not equal to "yes", fail (use guard)
-- else do not fail

getUserInfo :: WriterT Log (MaybeT IO) UserInfo
getUserInfo = undefined

getUserSalary :: WriterT Log (MaybeT IO) Int
getUserSalary = undefined
-- ask user if comfortable to share
-- get user answer
-- log user answer
-- return user answer
  

getUserName :: WriterT Log (MaybeT IO) String
getUserName = undefined
-- same as getUserSalary

getUserAddress :: WriterT Log (MaybeT IO) String
getUserAddress = undefined


-- runGetUserName = runMaybeT $ runWriterT getUserName (\x -> True)