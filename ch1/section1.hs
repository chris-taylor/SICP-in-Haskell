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
