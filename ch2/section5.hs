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

class Polynomial p where
    variable :: p a -> Char
    terms :: p a -> [Term a]
    makePoly :: Num a => Char -> [Term a] -> p a

data SparsePoly a = SP Char [Term a] deriving (Eq,Show)

instance Polynomial SparsePoly where
    variable (SP x _) = x
    terms (SP _ t) = t
    makePoly x t = SP x t

-- 2.88
instance (Polynomial p, Num a, Show (p a), Eq (p a)) => Num (p a) where
    p + q = addPoly p q
    p * q = mulPoly p q
    negate p = makePoly (variable p) (map negate (terms p))
    abs = undefined
    signum = undefined
    fromInteger n = makePoly 'x' [Term 0 (fromInteger n)]

addPoly :: (Polynomial p, Num a) => p a -> p a -> p a
addPoly p q = if variable p == variable q
    then makePoly (variable p) (addTerms (terms p) (terms q))
    else error "Polynomials not in the same variable -- ADDPOLY"

mulPoly :: (Polynomial p, Num a) => p a -> p a -> p a
mulPoly p q = if variable p == variable q
    then makePoly (variable p) (mulTerms (terms p) (terms q))
    else error "Polynomials not in the same variable -- MULPOLY"

addTerms :: Num a => [Term a] -> [Term a] -> [Term a]
addTerms xs [] = xs
addTerms [] ys = ys
addTerms (x:xs) (y:ys)
    | order x < order y = x : addTerms xs (y:ys)
    | order x > order y = y : addTerms (x:xs) ys
    | otherwise         = let z = x + y
        in if coef z == 0 then addTerms xs ys
                          else z : addTerms xs ys

mulTerms :: Num a => [Term a] -> [Term a] -> [Term a]
mulTerms []     ys = []
mulTerms (x:xs) ys = addTerms (mulTermByAllTerms x ys) (mulTerms xs ys)

mulTermByAllTerms :: Num a => Term a -> [Term a] -> [Term a]    
mulTermByAllTerms x []     = []
mulTermByAllTerms x (y:ys) = (x * y) : mulTermByAllTerms x ys

-- 2.89
data DensePoly a = DP Char [a] deriving (Eq,Show)

instance Polynomial DensePoly where
    variable (DP x _) = x
    terms (DP _ t) = map (\(x,n) -> Term n x) (zip t [0..])
    makePoly x t = DP x (makeTerms t [0..])
        where
            makeTerms []     _      = []
            makeTerms (t:ts) (n:ns) = if order t == n
                then (coef t) : makeTerms ts ns
                else (fromInteger 0) : makeTerms (t:ts) ns

