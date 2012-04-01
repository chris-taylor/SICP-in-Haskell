{-# LANGUAGE RankNTypes #-}

-- Define a Pair class with associated methods cons, car, cdr
class Pair p where
    cons :: a -> b -> p a b
    car :: p a b -> a
    cdr :: p a b -> b

instance Pair (,) where
    cons      = (,)
    car (a,_) = a
    cdr (_,b) = b

-- 2.1
data Rat a = Rat { numer :: a, denom :: a } deriving (Eq)

instance Show a => Show (Rat a) where
    show (Rat n d) = show n ++ "/" ++ show d

instance Integral a => Num (Rat a) where
    (Rat n1 d1) + (Rat n2 d2) = makeRat (n1 * d2 + n2 * d1) (d1 * d2)
    (Rat n1 d1) - (Rat n2 d2) = makeRat (n1 * d2 - n2 * d1) (d1 * d2)
    (Rat n1 d1) * (Rat n2 d2) = makeRat (n1 * n2) (d1 * d2)
    negate (Rat n d) = makeRat (negate n) d
    signum (Rat n d) = makeRat (signum $ n * d) 1
    abs (Rat n d) = makeRat (abs n) (abs d)
    fromInteger n = undefined

instance Integral a => Fractional (Rat a) where
    (Rat n1 d1) / (Rat n2 d2) = makeRat (n1 * d2) (n2 * d1)
    fromRational n = undefined

makeRat :: Integral a => a -> a -> Rat a
makeRat n d | d < 0  = Rat ((-n) `div` g) ((-d) `div` g)
            | d > 0  = Rat (n `div` g) (d `div` g)
            | d == 0 = error "Denominator cannot be zero!"
    where g = gcd n d

-- 2.2
data Line a = Line { start :: Point a, end :: Point a } deriving (Eq,Show)

data Point a = Point { getX :: a, getY :: a } deriving (Eq)

instance Show a => Show (Point a) where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

midpoint :: Fractional a => Line a -> Point a
midpoint (Line (Point x1 y1) (Point x2 y2)) = Point ((x1 + x2)/2) ((y1 + y2)/2)

-- 2.3
class Rectangle a where
    heightRect :: a -> Double
    widthRect :: a -> Double

    perimeter :: a -> Double
    perimeter rect = 2 * (heightRect rect + widthRect rect)

    area :: a -> Double
    area rect = heightRect rect * widthRect rect

data Rect = Rect { bottomLeft :: Point Double
                   , topRight :: Point Double } deriving (Eq,Show)

instance Rectangle Rect where
    heightRect (Rect (Point _ y1) (Point _ y2)) = y2 - y1
    widthRect  (Rect (Point x1 _) (Point x2 _)) = x2 - x1

data Rect' = Rect' { bottomLeft' :: Point Double
                   , heightRect' :: Double
                   , widthRect' :: Double } deriving (Eq,Show)

instance Rectangle Rect' where 
    heightRect (Rect' _ h _) = h
    widthRect  (Rect' _ _ w) = w

-- 2.4
data ChurchPair a b = CP (forall c. (a -> b -> c) -> c)

instance Pair ChurchPair where
    cons x y = CP (\m -> m x y)
    car (CP z) = z (\p q -> p)
    cdr (CP z) = z (\p q -> q)

-- 2.5
consi:: Integral a => a -> a -> a
consi a b = 2 ^ a * 3 ^ b

cari :: Integral a => a -> a
cari z = iter z 0
    where iter z n | z `rem` 2 == 0 = iter (z `div` 2) (n + 1)
                   | otherwise      = n

cdri :: Integral a => a -> a
cdri z = iter z 0
    where iter z n | z `rem` 3 == 0 = iter (z `div` 2) (n + 1)
                   | otherwise      = n

-- 2.6
zero :: (a -> a) -> (a -> a)
zero = \f -> id

add1 :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))
add1 n = \f -> f . (n f)

one :: (a -> a) -> (a -> a)
one = \f -> f

two :: (a -> a) -> (a -> a)
two = \f -> f . f

plus :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))
plus m n = \f -> (m f) . (n f)

-- Can check the above implementation of Church numerals using this function,
-- which should evaluate to True.
checkChurchEncoding :: Bool
checkChurchEncoding = 
    int (add1 zero) == int one &&
    int (add1 one) == int two &&
    int (add1 (add1 zero)) == int two &&
    int (add1 (add1 one)) == int (add1 two)
    where
        int n = (n (+1)) 0

-- 2.7 & 2.8
data Interval = Interval { lower :: Double, upper :: Double } deriving (Eq)

instance Show Interval where
    show (Interval a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

instance Num Interval where
    (+) = addi
    (-) = subi
    (*) = muli
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Fractional Interval where
    recip = recipi
    fromRational = undefined

addi :: Interval -> Interval -> Interval
addi (Interval a b) (Interval c d) = Interval (a + c) (b + d)

subi :: Interval -> Interval -> Interval
subi (Interval a b) (Interval c d) = Interval (a - d) (b - c)

muli :: Interval -> Interval -> Interval
muli (Interval a b) (Interval c d) =
    let p1 = a * c
        p2 = a * d
        p3 = b * c
        p4 = b * d
     in Interval (minimum [p1,p2,p3,p4]) (maximum [p1,p2,p3,p4])

recipi :: Interval -> Interval
recipi (Interval a b) = Interval (1 / b) (1 / a)

-- 2.9 n/a

-- 2.10
recipiSafe :: Interval -> Interval
recipiSafe (Interval a b) = if a < 0 && b > 0
    then error "Interval in denominator contains zero."
    else Interval (1 / b) (1 / a)

-- 2.11
fastMuli :: Interval -> Interval -> Interval
fastMuli (Interval a b) (Interval c d) = fmi a b c d
    where
        fmi a b c d | a >= 0 && c >= 0 = Interval (a * c) (b * d)

                    | a >= 0 && d >= 0 = Interval (b * c) (b * d)
                    | c >= 0 && b >= 0 = Interval (d * a) (d * b)

                    | b >= 0 && d >= 0 = Interval (min (b * c) (d * a))
                                                  (max (b * d) (a * c))

                    | a >= 0           = Interval (b * c) (d * a)
                    | c >= 0           = Interval (d * a) (b * c)

                    | b >= 0           = Interval (b * c) (a * c)
                    | d >= 0           = Interval (d * a) (a * c)

                    | otherwise        = Interval (b * d) (a * c)

-- 2.12
makeCenterPercent :: Double -> Double -> Interval
makeCenterPercent c p = Interval (c * (1 - p)) (c * (1 + p))

center :: Interval -> Double
center (Interval l u) = (l + u) / 2

percent :: Interval -> Double
percent (Interval l u) = (u - l) / (u + l)

-- 2.13 n/a
-- 2.14 n/a
-- 2.15 n/a
-- 2.16 n/a
