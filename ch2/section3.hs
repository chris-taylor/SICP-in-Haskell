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

