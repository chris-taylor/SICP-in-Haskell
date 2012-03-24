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