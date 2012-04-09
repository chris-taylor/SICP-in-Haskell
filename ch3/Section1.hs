-- Modularity, Objects and State

import Control.Monad.State
import Control.Monad.ST
import Control.Applicative
import Data.STRef
import System.Random

type Cash = Float
type Account = State Cash

withdraw' :: Cash -> Account (Either String Cash)
withdraw' amount = state makewithdrawal where
    makewithdrawal balance = if balance >= amount
        then (Right amount, balance - amount)
        else (Left "Insufficient funds", balance)

deposit' :: Cash -> Account ()
deposit' amount = state makedeposit where
    makedeposit balance = ((), balance + amount)

-- Thanks to Daniel Wagner on StackOverflow for explaining the ST monad
-- http://stackoverflow.com/questions/10048213/managing-state-chapter-3-of-sicp/10048527#10048527

makeWithdraw :: Cash -> ST s (Cash -> ST s (Either String Cash))
makeWithdraw initialBalance = do
    refBalance <- newSTRef initialBalance
    return $ \amount -> do
        balance <- readSTRef refBalance
        if amount > balance
            then return (Left "Insufficient funds")
            else do
                let newBalance = balance - amount
                writeSTRef refBalance newBalance
                return (Right $ newBalance)

testMakeWithdraw :: [Either String Cash]
testMakeWithdraw = runST $ do
    w1 <- makeWithdraw 100
    w2 <- makeWithdraw 100
    x1 <- w1 50
    y1 <- w2 70
    x2 <- w1 40
    y2 <- w2 40
    return [x1,y1,x2,y2]

-- Modelling accounts

type Action a = Cash -> (a, Cash)
type AccountOutput = Either String Cash

makeAccount :: Cash -> ST s (Action a -> ST s a)
makeAccount initialBalance = do
    refBalance <- newSTRef initialBalance
    return $ \f -> do
        balance <- readSTRef refBalance
        let (result, newBalance) = f balance
        writeSTRef refBalance newBalance
        return result

withdraw :: Cash -> Action AccountOutput
withdraw amount balance = if amount > balance
    then (Left "Insufficient funds", balance)
    else (Right newBalance, newBalance) where
        newBalance = balance - amount

deposit :: Cash -> Action AccountOutput
deposit amount balance = (Right newBalance, newBalance) where
    newBalance = balance + amount

testMakeAccount :: [AccountOutput]
testMakeAccount = runST $ do
    acc <- makeAccount 100
    mapM acc [ withdraw 50, withdraw 60, deposit 40, withdraw 60 ]

-- 3.1
makeAccumulator :: Num a => a -> ST s (a -> ST s a)
makeAccumulator initialValue = do
    refValue <- newSTRef initialValue
    return $ \x -> do
        modifySTRef refValue (+x)
        value <- readSTRef refValue
        return value

testMakeAccumulator :: Num a => a -> [a] -> [a]
testMakeAccumulator initialValue values = runST $ do
    acc <- makeAccumulator initialValue
    mapM acc values

-- 3.2
data MonitoredInput a = Input a | HowManyCalls

makeMonitored :: (a -> b) -> ST s (MonitoredInput a -> ST s (Either Integer b))
makeMonitored f = do
    refCount <- newSTRef 0
    return $ \x -> case x of
        Input x -> do
            modifySTRef refCount (+1)
            return (Right $ f x)
        HowManyCalls -> do
            count <- readSTRef refCount
            return (Left count)

testMakeMonitored :: (a -> b) -> [MonitoredInput a] -> [Either Integer b]
testMakeMonitored f inputs = runST $ do
    g <- makeMonitored f
    mapM g inputs

-- 3.3
type Password = String
type SecureAccount s a = (Password, Action a) -> ST s (Either String a)

makeSecureAccount :: Cash -> Password -> ST s (SecureAccount s a)
makeSecureAccount initialBalance password = do
    refBalance <- newSTRef initialBalance
    return $ \(pass,f) -> if pass /= password
        then return (Left $ "Incorrect Password: " ++ pass)
        else do
            balance <- readSTRef refBalance
            let (result, newBalance) = f balance
            writeSTRef refBalance newBalance
            return (Right result)

testMakeSecureAccount :: [Either String AccountOutput]
testMakeSecureAccount = runST $ do
    acc <- makeSecureAccount 100 "eggs"
    mapM acc [ ("eggs", withdraw 50), ("spam", withdraw 100), ("eggs", withdraw 100) ]


-- 3.4
makeSecureAccount' :: Cash -> Password -> ST s (SecureAccount s a)
makeSecureAccount' initialBalance password = do
    refBalance <- newSTRef initialBalance
    refCounter <- newSTRef 0
    return $ \(pass,f) -> if pass /= password
        then do
            modifySTRef refCounter (+1)
            count <- readSTRef refCounter
            if count < 7
                then return (Left $ "Incorrect Password: " ++ pass)
                else return (Left "Calling The Cops")
        else do
            balance <- readSTRef refBalance
            let (result, newBalance) = f balance
            writeSTRef refCounter 0
            writeSTRef refBalance newBalance
            return (Right result)

testMakeSecureAccount' :: [Either String AccountOutput]
testMakeSecureAccount' = runST $ do
    acc <- makeSecureAccount 100 "eggs"
    mapM acc [ ("spam",withdraw 100), ("ham", withdraw 100), 
        ("spinach", withdraw 100), ("muffin", withdraw 100),
        ("beans", withdraw 100), ("beer", withdraw 100), ("beef", withdraw 100) ]

-- Random numbers

makeRand :: Random a => Int -> ST s (t -> ST s a)
makeRand initialSeed = do
    genRef <- newSTRef (mkStdGen initialSeed)
    return $ \_ -> do
        gen <- readSTRef genRef
        let (result, newGen) = random gen
        writeSTRef genRef newGen
        return result

estimatePi :: Integer -> IO Double
estimatePi trials = do
    result <- monteCarlo trials cesaroTest
    return $ sqrt $ 6 / result

cesaroTest :: IO Bool
cesaroTest = do
    n <- randomIO :: IO Int
    m <- randomIO :: IO Int
    return (gcd n m == 1)

monteCarlo :: (Monad m, Fractional a) => Integer -> m Bool -> m a
monteCarlo trials experiment = iter trials 0 where
    iter remaining passed = if remaining == 0
        then return (fromInteger passed / fromInteger trials)
        else do 
            result <- experiment
            if result
                then iter (remaining - 1) (passed + 1)
                else iter (remaining - 1) passed

-- 3.5
estimateIntegral :: (Random a, Fractional a) => (a -> a -> Bool) -> a -> a -> a -> a -> Integer -> IO a
estimateIntegral p x1 x2 y1 y2 trials = monteCarlo trials experiment where
    experiment = p <$> randomRIO (x1,x2) <*> randomRIO (y1,y2)

estimatePi' :: Integer -> IO Double
estimatePi' trials = do
    result <- estimateIntegral (\x y -> x^2 + y^2 < 1) (-1) 1 (-1) 1 trials
    return (4 * result)

-- 3.6
data RandArg = Generate | Reset Int

makeRandReset :: (Random b, Num b) => Int -> ST s (RandArg -> ST s b)
makeRandReset initialSeed = do
    genRef <- newSTRef (mkStdGen initialSeed)
    return $ \arg -> case arg of
        Generate -> do
            gen <- readSTRef genRef
            let (result, newGen) = random gen
            writeSTRef genRef newGen
            return result
        Reset seed -> do
            writeSTRef genRef (mkStdGen seed)
            return 0

testRandReset :: [Int]
testRandReset = runST $ do
    rand <- makeRandReset 0 :: ST s (RandArg -> ST s Int)
    x <- rand Generate
    y <- rand Generate
    rand (Reset 0)
    x' <- rand Generate
    y' <- rand Generate
    return [x,y,x',y']

-- 3.7
makeJoint :: (SecureAccount s AccountOutput) -> Password -> Password -> ST s (SecureAccount s AccountOutput)
makeJoint acct oldPass newPass = do
        result <- acct (oldPass, testfun)
        case result of
            (Left _)  -> error "Passwords do not match."
            (Right _) -> return $ \(pass,f) -> if pass /= newPass
                then return (Left "Incorrect password")
                else acct (oldPass,f)
        where testfun bal = (Right bal,bal)

testMakeJoint :: [Either String AccountOutput]
testMakeJoint = runST $ do
    peter <- makeSecureAccount 100 "eggs"
    paul  <- makeJoint peter "eggs" "spam"
    x <- peter ("eggs", withdraw 50)
    y <- paul  ("spam", withdraw 50)
    z <- peter ("eggs", withdraw 50)
    return [x,y,z]

-- 3.8
makeF :: ST s (Integer -> ST s Integer)
makeF = do
    x <- newSTRef 0
    return $ \y -> do
        result <- readSTRef x
        if y == 1 then (writeSTRef x 1) else return ()
        return result

testLeftToRight :: Integer
testLeftToRight = runST $ do
    f <- makeF
    a <- f 0
    b <- f 1
    return (a + b)

testRightToLeft :: Integer
testRightToLeft = runST $ do
    f <- makeF
    b <- f 1
    a <- f 0
    return (a + b)

