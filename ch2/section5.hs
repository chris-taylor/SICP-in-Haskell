{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

{-  Complex number implementation, for polynomials with complex coefs. -}

data Complex = C Double Double deriving (Eq,Show)

instance Num Complex where
    (C a b) + (C c d) = C (a + c) (b + d)
    (C a b) - (C c d) = C (a - c) (b - d)
    (C a b) * (C c d) = C (a * c - b * d) (a * d + b * c)
    fromInteger n = C (fromInteger n) 0.0
    abs z = undefined
    signum z = undefined

{- Setup for polynomial arithmetic. -}

data Term a = Term { order :: Integer, coef :: a } deriving (Eq,Show)

instance Num a => Num (Term a) where
    (Term n x) + (Term m y) = if n == m then Term n (x + y) else error "Incompatible terms"
    (Term n x) - (Term m y) = if n == m then Term n (x - y) else error "Incompatible terms"
    (Term n x) * (Term m y) = Term (n + m) (x * y)
    negate (Term n x) = Term n (negate x)
    fromInteger n = Term 0 (fromInteger n)
    signum (Term n x) = Term n (signum x)
    abs (Term n x) = Term n (abs x)

data PolynomialWitness -- necessary to differentiate between Term and Polynomial

class Polynomial p where
    variable :: p a -> Char
    terms :: p a -> [Term a]
    makePoly :: Num a => Char -> [Term a] -> p a

data SparsePoly w a = SP Char [Term a] deriving (Eq,Show)

instance Polynomial (SparsePoly PolynomialWitness) where
    variable (SP x _) = x
    terms (SP _ t) = t
    makePoly x t = SP x t

-- 2.88
instance (Polynomial (p PolynomialWitness), Num a, Show (p PolynomialWitness a), Eq (p PolynomialWitness a)) => Num (p PolynomialWitness a) where
    p + q = addPoly p q
    p * q = mulPoly p q
    negate p = makePoly (variable p) (map negate (terms p))
    abs = undefined
    signum = undefined
    fromInteger n = makePoly 'x' [Term 0 (fromInteger n)]

addTerms :: Num a => [Term a] -> [Term a] -> [Term a]
addTerms xs [] = xs
addTerms [] ys = ys
addTerms (x:xs) (y:ys)
    | order x > order y = x : addTerms xs (y:ys)
    | order x < order y = y : addTerms (x:xs) ys
    | otherwise         = let z = x + y
        in if coef z == 0 then addTerms xs ys
                          else z : addTerms xs ys

addPoly :: (Polynomial p, Num a) => p a -> p a -> p a
addPoly p q = if variable p == variable q
    then makePoly (variable p) (addTerms (terms p) (terms q))
    else error "Polynomials not in the same variable -- ADDPOLY"

mulPoly :: (Polynomial p, Num a) => p a -> p a -> p a
mulPoly p q = if variable p == variable q
    then makePoly (variable p) (mulTerms (terms p) (terms q))
    else error "Polynomials not in the same variable -- MULPOLY"
    where
        mulTerms []     ys = []
        mulTerms (x:xs) ys = addTerms (mulTermByAllTerms x ys) (mulTerms xs ys)
        mulTermByAllTerms x []     = []
        mulTermByAllTerms x (y:ys) = (x * y) : mulTermByAllTerms x ys

negPoly :: (Polynomial p, Num a) => p a -> p a
negPoly p = makePoly (variable p) (map negate (terms p))

fromIntegerPoly :: (Polynomial p, Num a) => Integer -> p a
fromIntegerPoly n = makePoly 'x' [Term 0 (fromInteger n)]

-- 2.89
data DensePoly w a = DP Char [a] deriving (Eq,Show)

instance Polynomial (DensePoly PolynomialWitness) where
    variable (DP x _) = x
    terms (DP _ t) = reverse $ map (\(x,i) -> Term i x) (zip (reverse t) [0..])
    makePoly x [] = DP x []
    makePoly x t  = DP x (makeTerms t [n,n-1..0])
        where
            n = maximum (map order t)
            makeTerms []      ns    = replicate (length ns) 0
            makeTerms (t:ts) (n:ns) = if order t == n
                then (coef t) : makeTerms ts ns
                else (fromInteger 0) : makeTerms (t:ts) ns

--instance Num a => Num (DensePoly a) where
--    (+) = addPoly
--    (*) = mulPoly
--    negate = negPoly
--    fromInteger = fromIntegerPoly
--    abs = undefined
--    signum = undefined

-- 2.90
-- Probably needs to go in a separate file?

-- 2.91
divPoly :: (Polynomial p, Fractional a) => p a -> p a -> (p a, p a)
divPoly p q = if variable p == variable q
    then (makePoly (variable p) result, makePoly (variable p) remainder)
    else error "Polynomials not in the same variable -- DIVPOLY"
    where
        (result,remainder) = divTerms (terms p) (terms q)

divTerms :: Fractional a => [Term a] -> [Term a] -> ([Term a], [Term a])
divTerms []      ys    = ([], [])
divTerms (x:xs) (y:ys) = if order x < order y
    then ([], x:xs)
    else let newCoef = coef x / coef y
             newOrder = order x - order y
             firstTerm = Term newOrder newCoef
             rm = map (* negate firstTerm) ys
             (restOfResult, remainder) = divTerms (addTerms rm xs) (y:ys)
          in (firstTerm:restOfResult, remainder)

-- 2.92
-- To do.