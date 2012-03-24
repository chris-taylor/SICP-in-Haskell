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
double :: Num a => a -> a
double x = 2 * x

halve :: Integral a => a -> a
halve x = x `div` 2

mult :: Integral a => a -> a -> a
mult a b
    | b == 0 = 0
    | b == 1 = a
    | even b = double (a `mult` halve b)
    | True   = a + (a `mult` (b - 1))

-- 1.18
multIter :: Integral a => a -> a -> a
multIter a b = iter a b 0
    where iter a b res
            | b == 0 = res
            | even b = iter (double a) (halve b) res
            | True   = iter a (b - 1) (res + a)