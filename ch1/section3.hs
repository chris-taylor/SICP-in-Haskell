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

