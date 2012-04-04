import Data.Ratio

data Complex = C Double Double deriving (Eq,Show)

instance Num Complex where
    (C a b) + (C c d) = C (a + c) (b + d)
    (C a b) - (C c d) = C (a - c) (b - d)
    (C a b) * (C c d) = C (a * c - b * d) (a * d + b * c)
    fromInteger n = C (fromInteger n) 0.0
    abs z = undefined
    signum z = undefined

