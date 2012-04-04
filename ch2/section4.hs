{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

class Complex c where
    real :: RealFloat a => c a -> a
    imag :: RealFloat a => c a -> a
    magnitude :: RealFloat a => c a -> a
    angle :: RealFloat a => c a -> a
    fromRealImag :: RealFloat a => a -> a -> c a
    fromMagAng :: RealFloat a => a -> a -> c a

instance (Complex c, RealFloat a, Show (c a), Eq (c a)) => Num (c a) where
    w + z = fromRealImag (real w + real z) (imag w + imag z)
    w - z = fromRealImag (real w - real z) (imag w - imag z)
    w * z = fromMagAng (magnitude w * magnitude z) (angle w + angle z)
    fromInteger n = fromRealImag (fromInteger n) 0.0
    abs z = undefined
    signum z = undefined

instance (Complex c, RealFloat a, Show (c a), Eq (c a)) => Fractional (c a) where
    w / z = fromMagAng (magnitude w / magnitude z) (angle w - angle z)
    fromRational x = fromRealImag (fromRational x) 0.0

data ComplexRealImag a = CompRI a a deriving (Show,Eq)

instance Complex ComplexRealImag where
    real (CompRI x y) = x
    imag (CompRI x y) = y
    magnitude z = sqrt ((real z)^2 + (imag z)^2)
    angle z = atan2 (imag z) (real z)
    fromRealImag x y = CompRI x y
    fromMagAng r theta = CompRI (r * cos theta) (r * sin theta)

data ComplexPolar a = CompPolar a a deriving (Show,Eq)

instance Complex ComplexPolar where
    real z = magnitude z * cos (angle z)
    imag z = magnitude z * sin (angle z)
    magnitude (CompPolar r _) = r
    angle (CompPolar _ theta) = theta
    fromRealImag x y = CompPolar (sqrt (x^2 + y^2)) (atan2 y x)
    fromMagAng r theta = CompPolar r theta

-- 2.73

{-  Need to think a little harder about this one.

class Expr u where
    deriv :: Char -> u -> v

data Number = Num Double deriving (Eq,Show)
data Var = Var Char deriving (Eq,Show)
data Sum a b = Sum a b deriving (Eq,Show)
data Prod a b = Prod a b deriving (Eq,Show)

instance Expr Number where
    deriv _ n = Num 0.0

instance Expr Var where
    deriv x (Var y) = if x == y then Num 1.0 else Num 0.0

-}

-- 2.74 n/a

-- 2.75

{-  Basic symbol type for dispatch with O(1) comparison. -}

data Symbol = Symbol Int String

instance Show Symbol where
    show (Symbol _ s) = s

instance Eq Symbol where
    (Symbol n1 _) == (Symbol n2 _) = n1 == n2

{-  Here all of the work is done in the object, rather than in the instance
    declaration, which just does dispatch. The disadvantage is that because
    of the type system, 'method calls' all return the same type. In this case
    it works because all of the functions in the class Complex return RealFloat a,
    but you could be in trouble if you needed something more flexible.

    I don't think you would ever try to do something like this in Haskell, but
    I wanted to see if I could make the message-passing style work.

    An interesting consequence is that it is now possible to add two
    complex numbers using different internal representations, which was
    impossible in the previous code due to the type system. -}

instance Complex ((->)Symbol) where
    real = applyGeneric (Symbol 1 "real")
    imag = applyGeneric (Symbol 2 "imag")
    magnitude = applyGeneric (Symbol 3 "magnitude")
    angle = applyGeneric (Symbol 4 "angle")
    fromRealImag = newComplexFromRealImag
    fromMagAng = newComplexFromMagAng

instance RealFloat a => Show (Symbol -> a) where
    show x = show (real x) ++ " + " ++ show (imag x) ++ "i"

instance RealFloat a => Eq (Symbol -> a) where
    x == y = real x == real y && imag x == imag y

newComplexFromRealImag :: RealFloat a => a -> a -> (Symbol -> a)
newComplexFromRealImag x y = dispatch where
    dispatch msg = case msg of
        (Symbol 1 "real") -> x
        (Symbol 2 "imag") -> y
        (Symbol 3 "magnitude") -> sqrt(x^2 + y^2)
        (Symbol 4 "angle") -> atan2 y x

newComplexFromMagAng :: RealFloat a => a -> a -> (Symbol -> a)
newComplexFromMagAng r theta = dispatch where
    dispatch msg = case msg of
        (Symbol 1 "real") -> r * cos theta
        (Symbol 2 "imag") -> r * sin theta
        (Symbol 3 "magnitude") -> r
        (Symbol 4 "angle") -> theta

applyGeneric :: a -> (a -> b) -> b
applyGeneric op arg = arg op

-- 2.76 n/a
-- 2.77 n/a
-- 2.78 n/a
-- 2.79 n/a
-- 2.80 n/a

{-  The reason that most of these exercises aren't applicable is that type-
    tagging is built in to Haskell in the form of the type system. It seems
    pointless to try and replicate it in a language that already has types
    built in. -}
