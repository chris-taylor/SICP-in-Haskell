import System.Random (Random, randomRIO)
import System.CPUTime (getCPUTime)
import Control.Monad

-- 1.9
add :: (Enum a, Num a) => a -> a -> a
add 0 b = b
add a b = succ $ add (pred a) b

add' :: (Enum a, Num a) => a -> a -> a
add' 0 b = b
add' a b = add' (pred a) (succ b)

-- 1.10 n/a

-- 1.11
f :: Integral a => a -> a
f n = if n < 3
    then n
    else f (n-1) + 2 * f (n-2) + 3 * f (n-3)

f' :: Integral a => a -> a
f' n = fIter 2 1 0 n
    where
        fIter _ _ c 0 = c
        fIter a b c n = fIter (a + 2 * b + 3 * c) a b (n-1)

-- 1.12
pascal :: Integral a => a -> a -> a
pascal n k
    | k == 1 || k == n = 1
    | otherwise = pascal (n-1) (k-1) + pascal (n-1) k

-- 1.13 n/a

-- 1.14 n/a

-- 1.15 n/a

-- 1.16
square :: Num a => a -> a
square x = x * x

fastExpt :: (Num a, Integral b) => a -> b -> a
fastExpt b n = iter b n 1
    where iter b n res
            | n == 0 = res
            | even n = iter (square b) (n `div` 2) res
            | True   = iter b (n - 1) (b * res)

-- 1.17
mult :: Integral a => a -> a -> a
mult a b | b < 0 = -m a (-b)
         | True  =  m a b
         where m a b | b == 0 = 0
                     | even b = double (a `mult` halve b)
                     | True   = a + (a `mult` (b - 1))
               double x = 2 * x
               halve x  = x `div` 2

-- 1.18
multIter :: Integral a => a -> a -> a
multIter a b | b < 0 = -mi a (-b) 0
             | True  =  mi a b 0
             where mi a b res
                    | b == 0 = res
                    | even b = mi (2 * a) (b `div` 2) res
                    | True   = mi a (b - 1) (res + a)

-- 1.19
fib :: Integral a => a -> a
fib n = fibIter 1 0 0 1 n
    where
        fibIter a b p q cnt
            | cnt == 0 = b
            | even cnt = fibIter a
                                 b
                                 (p * p + q * q)
                                 (q * q + 2 * p * q)
                                 (cnt `div` 2)
            | True     = fibIter (b * q + a * q + q * p)
                                 (b * p + a * q)
                                 p
                                 q
                                 (cnt - 1)

-- 1.20 n/a

-- 1.21 n/a

-- 1.22
smallestDivisor :: Integral a => a -> a
smallestDivisor n = findDivisor n 2
    where
        findDivisor n test
            | test ^ 2 > n     = n
            | test `divides` n = test
            | otherwise        = findDivisor n (test + 1)
        divides a b = b `rem` a == 0

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = smallestDivisor n == n

timedPrimeTest :: Integral a => (a -> Bool) -> a -> IO ()
timedPrimeTest isPrime n = do
    putStr $ show n
    startTime <- getCPUTime
    startPrimeTest n startTime
    putStrLn ""
    where
        startPrimeTest n startTime = do
            if isPrime n
                then do
                    endTime <- getCPUTime
                    reportPrime (endTime - startTime)
                else return ()
        reportPrime elapsedTime = 
            putStr $ " *** " ++ show (fromIntegral elapsedTime / 1e12) ++ " seconds."

searchForPrimes :: Integral a => [a] -> [a]
searchForPrimes xs = filter isPrime xs

printThree :: Integral a => (a -> Bool) -> [a] -> IO ()
printThree isPrime xs = mapM_ (timedPrimeTest isPrime) $ take 3 $ searchForPrimes xs

inputs :: Integral a => [[a]]
inputs = [[10^3..],[10^4..],[10^5..],[10^10..],[10^11..],[10^12..]]

findPrimes :: IO ()
findPrimes = mapM_ (printThree isPrime) inputs

--isPrimeIO :: Integral a => a -> IO Bool
--isPrimeIO n = return (isPrime n)

-- 1.23
findPrimesFast :: IO ()
findPrimesFast = mapM_ (printThree isPrime) inputs
    where
        isPrime 1 = False
        isPrime n = smallestDivisor n == n
        smallestDivisor n = findDivisor n 2
        findDivisor n test | test ^ 2 > n     = n
                           | test `divides` n = test
                           | otherwise        = findDivisor n (next test)
        next 2 = 3
        next n = n + 2
        a `divides` b = b `rem` a == 0

-- 1.24
expMod :: Integral a => a -> a -> a -> a
expMod b n m
    | n == 0 = 1
    | even n = (expMod b (n `div` 2) m) ^ 2 `mod` m
    | True   = (b * expMod b (n-1) m) `mod` m

fermatTest :: Integral a => a -> a -> Bool
fermatTest n a = expMod a n n == a

fastPrime :: Integral a => [a] -> a -> Bool
fastPrime xs n = and $ map (fermatTest n) xs

getFermatIsPrime nTests = do
    randomNumbers <- sequence $ replicate nTests $ randomRIO (0 :: Integer, 1000)
    return (fastPrime randomNumbers)

findPrimesFermat :: IO ()
findPrimesFermat = do
    isPrime <- getFermatIsPrime 100
    mapM_ (printThree isPrime) inputs

{-  I'm not very happy with the solution to this. First, the type of
    getFermatIsPrime seems to be unnecessarily restricted. Second, it
    generates random numbers from 1..1000, meaning that it fails for
    inputs under 1000.

    I feel like there should be a way to generate
    random numbers in the range 1..n and use those for testing, but
    in findPrimesFermat I need a type of IO (t -> Bool) to bind to isPrime,
    and if I generate the random numbers *after* taking in the input n, then
    the type will be IO (t -> IO Bool) because some IO has to be done after
    we look at the input.

    Perhaps the way around it is to use randomR
    instead of randomRIO, which requires me to deal with the generator
    explicitly, but has the advantage that it avoids wrapping everything
    up in an IO monad.

    Below are my earlier attempts; I don't think they will work though.

    fermatTest :: (Random t, Integral t) => t -> IO Bool
    fermatTest n = do
            rand <- randomRIO (1, n - 1)
            return (tryIt rand)
        where
            tryIt a = expMod a n n == a

    fastPrime :: (Random t, Integral t) => Int -> t -> IO Bool
    fastPrime times n = do
        if times == 0
            then return True
            else do
                t <- fermatTest n
                if t
                    then fastPrime (times - 1) n
                    else return False

    findPrimesFermat = do
        isPrime <- fastPrime 100
        mapM_ (printThree isPrime) inputs
-}

-- 1.25 n/a
-- 1.26 n/a

-- 1.27
passFermatTest :: Integral a => a -> Bool
passFermatTest n = and $ map (fermatTest n) [0..(n-1)]

isCarmichael :: Integral a => a -> Bool
isCarmichael n = passFermatTest n && not (isPrime n)

-- 1.28
expMod' :: Integral a => a -> a -> a -> a
expMod' b n m
    | n == 0 = 1
    | even n = let s = expMod b (n `div` 2) m
                   t = s ^ 2 `mod` m
                in if s /= 1 && s /= n - 1 && t == 1
                    then (-1)
                    else t
    | True   = (b * expMod b (n-1) m) `mod` m

millerRabinTest :: Integral a => a -> a -> Bool
millerRabinTest n a = b == a && b /= (-1)
    where b = expMod' a n n

fastMillerRabinPrime :: Integral a => [a] -> a -> Bool
fastMillerRabinPrime xs n = and $ map (millerRabinTest n) xs

getMillerRabinIsPrime nTests = do
    randomNumbers <- sequence $ replicate nTests $ randomRIO (0 :: Integer, 1000)
    return (fastMillerRabinPrime randomNumbers)

findPrimesMillerRabin :: IO ()
findPrimesMillerRabin = do
    isPrime <- getMillerRabinIsPrime 100
    mapM_ (printThree isPrime) inputs

