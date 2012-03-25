import Prelude hiding (sum)

-- 1.29
sum :: (Ord a, Num a) => (a -> a) -> a -> (a -> a) -> a -> a
sum term a next b = if a > b
    then 0
    else term a + sum term (next a) next b

integral :: (Ord a, Fractional a) => (a -> a) -> a -> a -> a -> a
integral f a b dx = dx * sum f (a + dx / 2) addDx b
    where addDx x = x + dx

