{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, RankNTypes, ImpredicativeTypes #-}

import Data.Ratio

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
    makePoly x [] = SP x [0]
    makePoly x t  = SP x t

-- 2.88
reduce :: (Polynomial p, Num a) => p a -> p a
reduce p = makePoly (variable p) (filter (\t -> coef t /= 0) (terms p))

instance (Polynomial (p PolynomialWitness), Num a, Ord a, Show (p PolynomialWitness a), Eq (p PolynomialWitness a)) => Num (p PolynomialWitness a) where
    (+) = addPoly
    (*) = mulPoly
    negate = negPoly
    abs = absPoly
    signum = signumPoly
    fromInteger n = makePoly 'x' [Term 0 (fromInteger n)]

addTerms :: Num a => [Term a] -> [Term a] -> [Term a]
addTerms xs [] = xs
addTerms [] ys = ys
addTerms (x:xs) (y:ys)
    | order x > order y = x : addTerms xs (y:ys)
    | order x < order y = y : addTerms (x:xs) ys
    | otherwise         = (x + y) : addTerms xs ys

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

absPoly :: (Polynomial p, Num a, Ord a) => p a -> p a
absPoly p = if (coef . head . terms $ p) < 0
    then negPoly p
    else p

signumPoly :: (Polynomial p, Num a) => p a -> p a
signumPoly p = makePoly (variable p) [Term 0 (signum . coef . head . terms $ p)]

fromIntegerPoly :: (Polynomial p, Num a) => Integer -> p a
fromIntegerPoly n = makePoly 'x' [Term 0 (fromInteger n)]

-- 2.89
data DensePoly w a = DP Char [a] deriving (Eq,Show)

instance Polynomial (DensePoly PolynomialWitness) where
    variable (DP x _) = x
    terms (DP _ t) = reverse $ map (\(x,i) -> Term i x) (zip (reverse t) [0..])
    makePoly x [] = DP x [0]
    makePoly x t  = DP x (makeTerms t [n,n-1..0])
        where
            n = maximum (map order t)
            makeTerms []      ns    = replicate (length ns) 0
            makeTerms (t:ts) (n:ns) = if order t == n
                then (coef t) : makeTerms ts ns
                else (fromInteger 0) : makeTerms (t:ts) ns

-- 2.90
-- Probably needs to go in a separate file?

-- 2.91
divModPoly :: (Polynomial p, Fractional a) => p a -> p a -> (p a, p a)
divModPoly p q = if variable p == variable q
    then (makePoly (variable p) result, makePoly (variable p) remainder)
    else error "Polynomials not in the same variable -- DIVPOLY"
    where
        (result,remainder) = divModTerms (terms p) (terms q)

divModTerms :: Fractional a => [Term a] -> [Term a] -> ([Term a], [Term a])
divModTerms []      ys    = ([], [])
divModTerms (x:xs) (y:ys) = if order x < order y
    then ([], x:xs)
    else let newCoef = coef x / coef y
             newOrder = order x - order y
             firstTerm = Term newOrder newCoef
             rm = map (* negate firstTerm) ys
             (restOfResult, remainder) = divModTerms (addTerms rm xs) (y:ys)
          in (firstTerm:restOfResult, remainder)

-- 2.92
-- To do.

-- 2.93 && 2.94
type RationalFunction a = Ratio (Polynomial p => p a)

degree :: Polynomial p => p a -> Integer
degree p = maximum $ map order $ terms p

instance (Polynomial (p PolynomialWitness), Eq (p PolynomialWitness a)) => Ord (p PolynomialWitness a) where
    compare p q = compare (degree p) (degree q)

instance (Polynomial (p PolynomialWitness), Num (p PolynomialWitness a), Ord (p PolynomialWitness a)) => Real (p PolynomialWitness a) where
    toRational = toRational . degree

instance (Num a, Polynomial (p PolynomialWitness)) => Enum (p PolynomialWitness a) where
    toEnum n = makePoly 'x' [Term (toInteger n) 1]
    fromEnum = fromInteger . degree

instance (Fractional a, Polynomial (p PolynomialWitness), Real (p PolynomialWitness a), Enum (p PolynomialWitness a)) => Integral (p PolynomialWitness a) where
    divMod = divModPoly
    quotRem = divModPoly
    toInteger = degree

{-  To make polynomials an instance of Rational it is necessary to make them into
    an instance of Integral. This involves a lot of boilerplate, but otherwise
    it isn't too hard.

    Note that for the Euclidean algorithm to be well defined on a domain R, all
    that is necessary is that we have addition, subtraction and commutative
    multiplication on R, as well as a we of assigning x in R to an integer m x
    such that m (x * y) >= m x for all nonzero x and y. For integers we have
    m = abs, and for polynomials we have m = degree.

    We can now write e.g.

        r = p % q

    for two polynomials p and q, and be sure that arithmetic operations will
    be performed correctly, and the reduction to lowest terms will be carried
    out correctly. -}

-- 2.95 
