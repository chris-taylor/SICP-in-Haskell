import Data.List (nub,sort)

-- 2.54
isEqual :: Eq a => [a] -> [a] -> Bool
isEqual (x:xs) [] = False
isEqual [] (y:ys) = False
isEqual [] []     = True
isEqual (x:xs) (y:ys) = x == y && isEqual xs ys

-- 2.55 n/a

-- 2.56
data Expr = Num Double
          | Var Char
          | Sum Expr Expr
          | Prod Expr Expr
          | Expt Expr Double
          deriving (Eq)

instance Show Expr where
    show (Num x) = show x
    show (Var x) = [x]
    show (Sum x y)  = "(+ " ++ show x ++ " " ++ show y ++ ")"
    show (Prod x y) = "(* " ++ show x ++ " " ++ show y ++ ")"
    show (Expt x n) = "(^ " ++ show x ++ " " ++ show n ++ ")"

--takeUntil :: Eq a => a -> [a] -> [a]
--takeUntil v []     = []
--takeUntil v (x:xs) = if x == v
--    then []
--    else x : takeUntil v xs

--splitOn :: Eq a => a -> [a] -> ([a], [a])
--splitOn v (x:xs) = if x == v
--    then ([], xs)
--    else (x:hd,tl) where (hd,tl) = splitOn v xs

--instance Read Expr where
--    read (hd:tl) = if hd == "("
--        then makeExpr op arg1 arg2
--        else error "Expressions must start with a parenthesis"
--        where
--            (op,rest) = splitOn ' ' tl
--            (arg1,rest') = splitOn ' ' rest
--            (arg2,_) = splitOn ')' 

makeSum :: Expr -> Expr -> Expr
makeSum (Num x) (Num y) = Num (x + y)
makeSum x (Num 0.0)     = x
makeSum (Num 0.0) x     = x
makeSum x y             = Sum x y

makeProd :: Expr -> Expr -> Expr
makeProd (Num x) (Num y) = Num (x * y)
makeProd _ (Num 0.0)     = Num 0.0
makeProd (Num 0.0) _     = Num 0.0
makeProd x (Num 1.0)     = x
makeProd (Num 1.0) x     = x
makeProd x y             = Prod x y

makeExpt :: Expr -> Double -> Expr
makeExpt x 0.0 = Num 1.0
makeExpt x 1.0 = x
makeExpt x n   = Expt x n

deriv :: Char -> Expr -> Expr
deriv x expr = d expr where
    d (Var y)    = if y == x then Num 1.0 else Num 0.0
    d (Num _)    = Num 0.0
    d (Sum f g)  = makeSum (d f) (d g)
    d (Prod f g) = makeSum (makeProd f (d g)) (makeProd (d f) g)
    d (Expt f n) = makeProd (Num n) (makeProd (makeExpt f (n-1)) (d f))

-- 2.57 n/a
-- Functions in Haskell can't handle multiple arguments.

-- 2.58
-- Handle infix as well as prefix notation. To do this I plan to make Expr an
-- instance of Read so that I can type in expressions as strings.

-- 2.59
class Set s where
    isElementOf :: (Eq a, Ord a) => a -> s a -> Bool
    adjoin :: (Eq a, Ord a) => a -> s a -> s a
    intersect :: (Eq a, Ord a) => s a -> s a -> s a
    union :: (Eq a, Ord a) => s a -> s a -> s a

instance Set [] where

    y `isElementOf` []     = False
    y `isElementOf` (x:xs) = if y == x
        then True
        else y `isElementOf` xs 

    y `adjoin` set = if y `isElementOf` set
        then set
        else y : set

    []     `intersect` xs = []
    (y:ys) `intersect` xs = if y `isElementOf` xs
        then y : (ys `intersect` xs)
        else ys `intersect` xs

    []     `union` xs = xs
    (y:ys) `union` xs = if y `isElementOf` xs
        then (ys `union` xs)
        else y : (ys `union` xs)

-- 2.60
data Bag a = Bag [a] deriving (Show)

instance Set Bag where

    y `isElementOf` Bag []     = False
    y `isElementOf` Bag (x:xs) = if y == x
        then True
        else y `isElementOf` Bag xs

    y `adjoin` Bag xs = Bag (y:xs)

    Bag []     `intersect` bag = Bag []
    Bag (y:ys) `intersect` bag = if y `isElementOf` bag
        then y `adjoin` (Bag ys `intersect` bag)
        else Bag ys `intersect` bag

    Bag ys `union` Bag xs = Bag (ys ++ xs)

-- 2.61 && 2.62
data OrderedList a = OL [a] deriving (Eq,Show)

instance Set OrderedList where

    y `isElementOf` OL []     = False
    y `isElementOf` OL (x:xs)
        | y == x = True
        | y < x  = False
        | otherwise = y `isElementOf` OL xs

    y `adjoin` OL xs = OL (y `insert` xs) where
        y `insert` [] = [y]
        y `insert` (x:xs)
            | y == x = x:xs
            | y < x  = y:x:xs
            | y > x  = x : (y `insert` xs)

    OL xs `intersect` OL ys = OL (xs `oi` ys) where
        []     `oi` ys = []
        xs     `oi` [] = []
        (x:xs) `oi` (y:ys)
            | x == y = x : (xs `oi` ys)
            | x < y  = xs `oi` (y:ys)
            | y < x  = (x:xs) `oi` ys

    OL xs `union` OL ys = OL (xs `ou` ys) where
        [] `ou` xs = xs
        xs `ou` [] = xs
        (x:xs) `ou` (y:ys)
            | y == x = x : (xs `ou` ys)
            | x < y  = x : (xs `ou` (y:ys))
            | x > y  = y : ((x:xs) `ou` ys)

-- 2.63 n/a
-- 2.64 n/a

-- 2.65
data BinaryTree a = Nil | Tree a (BinaryTree a) (BinaryTree a) deriving (Eq)

instance Show a => Show (BinaryTree a) where
    show Nil = "."
    show (Tree x l r) ="(" ++ show x ++ " " ++ show l ++ " " ++ show r ++ ")"

instance Set BinaryTree where

    y `isElementOf` Nil = False
    y `isElementOf` Tree x l r
        | y == x = True
        | y < x  = y `isElementOf` l
        | y > x  = y `isElementOf` r

    y `adjoin` Nil = Tree y Nil Nil
    y `adjoin` Tree x l r
        | y == x = Tree x l r
        | y < x  = Tree x (y `adjoin` l) r
        | y > x  = Tree x l (y `adjoin` r)

    xs `intersect` ys = toTree $ (toList xs) `intersect` (toList ys)

    xs `union` ys = toTree $ (toList xs) `union` (toList ys)

toList :: BinaryTree a -> [a]
toList  Nil         = []
toList (Tree x l r) = (toList l) ++ (x : toList r)

toList' :: BinaryTree a -> [a]
toList' t = copyToList t [] where
    copyToList  Nil         result = result
    copyToList (Tree x l r) result = copyToList l (x : copyToList r result)

toTree :: [a] -> BinaryTree a
toTree xs = fst $ partialTree xs (length xs) where
    partialTree xs n = if n == 0
        then (Nil, xs)
        else let leftSize  = (n - 1) `quot` 2
                 rightSize = n - leftSize - 1
                 (leftTree, y:ys)  = partialTree xs leftSize
                 (rightTree, rest) = partialTree ys rightSize
              in (Tree y leftTree rightTree, rest)

