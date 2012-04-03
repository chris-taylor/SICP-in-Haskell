{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

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