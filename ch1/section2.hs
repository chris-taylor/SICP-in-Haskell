import System.Random (Random, randomRIO)
import System.CPUTime (getCPUTime)

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
timedPrimeTest n = do
    putStr $ show n
    startTime <- getCPUTime
    startPrimeTest n startTime

startPrimeTest n startTime = if isPrime n
    then do
        endTime <- getCPUTime
        let elapsedTime = (fromIntegral $ endTime - startTime) / 1e12
        reportPrime elapsedTime
    else putStrLn ""

reportPrime elapsedTime = do
    putStrLn $ " *** " ++ show elapsedTime ++ " seconds."

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

expMod :: Integral a => a -> a -> a -> a
expMod b n m
    | n == 0 = 1
    | even n = (square $ expMod b (n `div` 2) m) `mod` m
    | True   = (b * expMod b (n-1) m) `mod` m

fermatTest :: (Random t, Integral t) => t -> IO Bool
fermatTest n = let try a = a == expMod a n n
    in do
        r <- randomRIO (1, n)
        return $ try r

fastPrime :: (Random t, Integral t) => t -> t -> IO Bool
fastPrime n times = do
    if times == 0
        then return True
        else do
            t <- fermatTest n
            if t
                then fastPrime n (times - 1)
                else return False
