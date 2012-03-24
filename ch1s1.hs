-- 1.1 n/a

-- 1.2
ex1_2 :: Fractional a => a
ex1_2 = (/) ((+) 5
                 ((+) 4
                      ((-) 2
                           ((-) 3
                                ((+) 6
                                     ((/) 4 5))))))
            ((*) 3
                 ((*) ((-) 6 2)
                      ((-) 2 7)))

-- 1.3
ex1_3 :: (Num a, Ord a) => a -> a -> a -> a
ex1_3 a b c
    | c < a && c < b = a * a + b * b
    | b < a && b < c = a * a + c * c
    | otherwise      = b * b + c * c

-- 1.4 n/a

-- 1.5 n/a

-- 1.6 n/a

-- 1.7
sqrtRel :: (Ord a, Floating a) => a -> a
sqrtRel x = sqrtIter 1.0 2.0 x
    where
        sqrtIter g1 g2 x = if goodEnough g1 g2
            then g1
            else sqrtIter (improve g1) g1 x
        goodEnough g1 g2 = abs (g1 / g2 - 1) < 0.001
        improve g1 = average g1 (x / g1)

average :: Floating a => a -> a -> a
average x y = (x + y) / 2

-- 1.8
cbrt :: (Ord a, Floating a) => a -> a
cbrt x = cbrtIter 1.0 x
    where
        cbrtIter guess x = if goodEnough guess
            then guess
            else cbrtIter (improve guess) x
        goodEnough guess = abs (guess * guess * guess / x - 1) < 1e-8
        improve guess = (2 * guess + x / (guess * guess)) / 3

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
