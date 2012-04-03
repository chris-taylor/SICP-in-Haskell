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
isElementOf :: Eq a => a -> [a] -> Bool
y `isElementOf` []     = False
y `isElementOf` (x:xs) = if y == x then True
                                   else y `isElementOf` xs 

adjoin :: Eq a => a -> [a] -> [a]
y `adjoin` set = if y `isElementOf` set then set
                                        else y : set

intersect :: Eq a => [a] -> [a] -> [a]
[]     `intersect` xs = []
(y:ys) `intersect` xs = if y `isElementOf` xs
    then y : (ys `intersect` xs)
    else ys `intersect` xs

union :: Eq a => [a] -> [a] -> [a]
[]     `union` xs = xs
(y:ys) `union` xs = if y `isElementOf` xs
    then (ys `union` xs)
    else y : (ys `union` xs)

    