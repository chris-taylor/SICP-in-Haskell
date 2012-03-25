--import Prelude hiding (sum)

-- 1.29
sum' :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sum' term a next b = if a > b
    then 0
    else term a + sum' term (next a) next b

integral :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> a
integral f a b dx = dx * sum' f (a + dx / 2) addDx b
    where addDx x = x + dx

cube :: Num a => a -> a
cube x = x * x * x

simpson :: Fractional a => (a -> a) -> a -> a -> Int -> a
simpson f a b n = (h / 3) * sum' term 0 (+1) n
    where term k | k == 0 || k == n = g k
                 | even k           = 2 * g k
                 | odd k            = 4 * g k
          g k = f (a + h * fromIntegral k)
          h   = (b - a) / fromIntegral n

--simpson :: Fractional a => (a -> a) -> a -> a -> Int -> a
--simpson f a b n = (h / 3) * (sum $ map y [0..n])
--    where y k | k == 0 || k == n = g k
--              | even k           = 2 * g k
--              | odd k            = 4 * g k
--          g k = f (a + h * fromIntegral k)
--          h = (b - a) / fromIntegral n