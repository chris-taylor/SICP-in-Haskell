import Prelude hiding (sqrt)

-- 1.29
sumRec :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumRec term a next b = if a > b
    then 0
    else term a + sumRec term (next a) next b

integral :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> a
integral f a b dx = dx * sumRec f (a + dx / 2) addDx b
    where addDx x = x + dx

simpson :: Fractional a => (a -> a) -> a -> a -> Int -> a
simpson f a b n = (h / 3) * sumRec term 0 (+1) n
    where term k | k == 0 || k == n = g k
                 | even k           = 2 * g k
                 | odd k            = 4 * g k
          g k = f (a + h * fromIntegral k)
          h   = (b - a) / fromIntegral n

-- 1.30
sumIter :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumIter term a next b = iter a 0
    where
        iter a result = if a > b 
            then result
            else iter (next a) (result + term a)

-- 1.31
prodRec :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
prodRec term a next b = if a > b
    then 1
    else term a * prodRec term (next a) next b

factorial :: (Ord a, Num a) => a -> a
factorial n = prodRec id 1 (+1) n

piApprox :: Fractional a => Int -> a
piApprox n = 4 * prodRec f 1 (+1) n
    where
        f n     = fromIntegral (numer n) / fromIntegral (denom n)
        numer n | even n = n + 2
                | odd  n = n + 1
        denom n | even n = n + 1
                | odd  n = n + 2

prodIter :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
prodIter term a next b = iter a 1
    where
        iter a result = if a > b
            then result
            else iter (next a) (result * term a)

-- 1.32
accumulate :: Ord a => (b -> c -> c) -> c -> (a -> b) -> a -> (a -> a) -> a -> c
accumulate op nullValue term a next b = if a > b
    then nullValue
    else (term a) `op` (accumulate op nullValue term (next a) next b)

sumAccum :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumAccum term a next b = 
    accumulate (+) 0 term a next b

prodAccum :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
prodAccum term a next b = 
    accumulate (*) 1 term a next b

accumIter :: Ord a => (b -> c -> c) -> c -> (a -> b) -> a -> (a -> a) -> a -> c
accumIter combiner nullValue term a next b = iter a nullValue
    where
        iter a result = if a > b
            then result
            else iter (next a) (combiner (term a) result)

-- 1.33
filteredAccumulate :: Ord a => (b -> c -> c) -> c -> (a -> Bool) -> (a -> b) -> a -> (a -> a) -> a -> c
filteredAccumulate op nullValue cond term a next b = if a > b
    then nullValue
    else if cond a
        then (term a) `op` rest
        else rest
        where
            rest = filteredAccumulate op nullValue cond term (next a) next b

-- a. (uses isPrime from Section 2)
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = smallestDivisor n == n
    where
        smallestDivisor n = findDivisor n 2
        findDivisor n test | test ^ 2 > n     = n
                           | test `divides` n = test
                           | otherwise        = findDivisor n (test + 1)
        divides a b = b `rem` a == 0

sumOfPrimeSquares :: Integral a => a -> a -> a
sumOfPrimeSquares a b = filteredAccumulate (+) 0 isPrime (^2) a succ b

-- b. 
productOfCoprimes :: Integral a => a -> a
productOfCoprimes n = filteredAccumulate (*) 1 coPrime id 1 succ n
    where coPrime m = gcd m n == 1

-- 1.34 n/a

-- 1.35
fixedPoint :: (Ord a, Fractional a) => (a -> a) -> a -> a
fixedPoint f guess = try guess
    where
        try guess = let next = f guess in
            if closeEnough guess next
                then next
                else try next
        closeEnough v1 v2 = abs (v1 - v2) < tolerance
        tolerance = 0.00001

goldenRatio :: (Ord a, Floating a) => a
goldenRatio = fixedPoint (\x -> 1 + 1/x) 1.0

-- 1.36
fixedPointVerbose :: (Ord a, Fractional a) => (a -> a) -> a -> IO a
fixedPointVerbose f guess = try guess 1
    where
        try guess n = do
            putStrLn $ "Guess " ++ show n ++ ": " ++ show guess
            let next = f guess
            if closeEnough guess next
                then return next
                else try next (n + 1)
        closeEnough v1 v2 = abs (v1 - v2) < tolerance
        tolerance = 0.00001

xToXIs1000 :: (Ord a, Floating a) => IO a
xToXIs1000 = fixedPointVerbose (\x -> log 1000 / log x) 2.0

xToXIs1000AvgDamp :: (Ord a, Floating a) => IO a
xToXIs1000AvgDamp = fixedPointVerbose (\x -> (x + log 1000 / log x) / 2) 2.0

-- 1.37
-- a.
contFrac :: (Integral i, Fractional a) => (i -> a) -> (i -> a) -> i -> a
contFrac n d k = helper n d 1
    where
        helper n d i = if i == k
            then n i / d i
            else n i / (d i + helper n d (i+1))

goldenRatioContFrac :: (Integral i, Fractional a) => i -> a
goldenRatioContFrac k = 1 / contFrac (\i -> 1.0) (\i -> 1.0) k

-- b.
contFracIter :: (Integral i, Fractional a) => (i -> a) -> (i -> a) -> i -> a
contFracIter n d k = iter k 0
    where
        iter i result = if i == 0
            then result
            else iter (i-1) (n i / (d i + result))

-- 1.38
e :: (Integral i, Fractional a) => i -> a
e k = 2 + contFrac n d k
    where n i = 1.0
          d i = if i `mod` 3 == 2
            then 2 * (fromIntegral i + 1) / 3
            else 1.0

-- 1.39
tanCF :: (Integral i, Fractional a) => a -> i -> a
tanCF x k = contFrac n d k
    where n 1 = x
          n i = -(x^2)
          d i  = 2 * fromIntegral i - 1

-- 1.40
newtonTransform :: Fractional a => (a -> a) -> (a -> a)
newtonTransform g = \x -> x - g x / deriv g x
    where deriv g x = (g (x+dx) - g x) / dx
          dx = 0.00001

newtonsMethod :: (Ord a, Fractional a) => (a -> a) -> a -> a
newtonsMethod g guess = fixedPoint (newtonTransform g) guess

cubic :: Num a => a -> a -> a -> a -> a
cubic a b c x = x^3 + a * x^2 + b * x + c

-- 1.41
double :: (a -> a) -> (a -> a)
double f x = f (f x)

{-  Since composition is built in to Haskell, it's even easier to write
        double f = f . f
    but that feels like cheating. -}

-- 1.42
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

{-  Similarly to above, I could define
        compose = (.)
    but again that feels too easy. -}

-- 1.43
repeated :: Integral i => (a -> a) -> i -> (a -> a)
repeated f n = if n == 0
    then id
    else compose f $ repeated f (n-1)

-- 1.44
smooth :: Fractional a => a -> (a -> a) -> (a -> a)
smooth dx f x = (f (x+dx) + f x + f (x-dx)) / 3

repeatedSmooth :: (Integral i, Fractional a) => i -> a -> (a -> a) -> (a -> a)
repeatedSmooth n dx = repeated (smooth dx) n

-- 1.45
averageDamp :: Fractional a => (a -> a) -> (a -> a)
averageDamp f = \x -> (x + f x) / 2

logi :: Integral a => a -> a
logi n = iter n 0
    where iter n result = if n == 1
            then result
            else iter (n `div` 2) (result + 1)

nthRoot :: (Integral a, Ord b, Fractional b) => a -> b -> b
nthRoot n x = fixedPoint (repeated averageDamp k f) 1.0
    where k = logi n
          f = \y -> x / y ^ (n - 1)

-- 1.46
iterativeImprove :: (a -> Bool) -> (a -> a) -> a -> a
iterativeImprove goodEnough improve guess = try guess
    where
        try guess = if goodEnough guess
            then guess
            else try (improve guess)

sqrtII :: (Ord a, Fractional a) => a -> a
sqrtII x = iterativeImprove goodEnough g 1.0
    where
        goodEnough guess = abs (guess^2 - x) < 0.0001
        g y = (y + x/y) / 2

fixedPointII :: (Ord a, Fractional a) => (a -> a) -> a -> a
fixedPointII f guess = iterativeImprove goodEnough f guess
    where
        goodEnough guess = abs (guess - f guess) < tolerance
        tolerance = 0.00001
