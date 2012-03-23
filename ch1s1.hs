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