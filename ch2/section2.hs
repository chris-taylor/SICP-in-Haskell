import Data.List (intersperse,nub)

{-  Scheme lists using Cons, car, cdr. Probably I should just embrace
    Haskell and use the built in functions
        (:)  <==> cons
        head <==> car
        tail <==> cdr
    instead, but it feels important to keep something of the flavour of
    Scheme. -}

data Cons a = Nil | Cons a (Cons a) deriving (Eq)

instance Show a => Show (Cons a) where
    show x = "(" ++ str x ++ ")"
        where
            str  Nil         = ""
            str (Cons x Nil) = show x
            str (Cons x y)   = show x ++ " " ++ str y

car :: Cons a -> a
car Nil = error "Can't take the car of Nil"
car (Cons x _) = x

cdr :: Cons a -> Cons a
cdr Nil = error "Can't take the cdr of Nil"
cdr (Cons _ y) = y

cadr = car . cdr
cddr = cdr . cdr

list :: [a] -> Cons a
list []     = Nil
list (x:xs) = Cons x (list xs)

-- 2.17

{-  At the moment this only operates on lists of the same type. Hopefully I can
    figure out a way to make it operate on lists of objects of different types
    Maybe it's possible to wrap everything up in something like

        data SchemeObject = SO (forall a. Show a => a)

    Another option would be to define

        data SchemeObject = SchemeNum Double
                          | SchemeString String
                          | SchemeBool Bool
                          | ...

    but this is getting close to an "informally specified, bug-ridden
    implementation of half of Common Lisp." -}

lastPair :: Cons a -> Cons a
lastPair p@(Cons x Nil) = p
lastPair   (Cons x y)   = lastPair y

-- 2.18
reverseList :: Cons a -> Cons a
reverseList xs = iter xs Nil where
    iter  Nil result       = result
    iter (Cons x y) result = iter y (Cons x result)

-- 2.19
usCoins :: [Integer]
usCoins = [50,25,10,5,1]

ukCoins :: [Integer]
ukCoins = [100,50,20,10,5,2,1]

cc :: (Ord a, Num a, Num b) => a -> [a] -> b
cc amount coinValues
    | amount == 0                     = 1
    | amount < 0 || noMore coinValues = 0
    | otherwise =
        (cc amount (exceptFirstDenomination coinValues)) + 
        (cc (amount - (firstDenomination coinValues)) coinValues)
    where
        firstDenomination = head
        exceptFirstDenomination = tail
        noMore [] = True
        noMore xs = False

-- 2.20

{-  Haskell functions can't accept a variable number of arguments (it wouldn't
    play nice with the type system) but since this function expects integers,
    I figure it's okay for it to accept a list instead. -}

sameParity :: Integral a => [a] -> [a]
sameParity (x:xs) = x : fun xs where
    fun [] = []
    fun (y:ys) | parityMatch y = y : fun ys
               | otherwise     = fun ys
    parityMatch y = even (x - y)

-- 2.21
squareList :: Num a => [a] -> [a]
squareList []     = []
squareList (x:xs) = (x^2) : squareList xs

squareList' :: Num a => [a] -> [a]
squareList' xs = map (^2) xs

-- 2.22 n/a
-- Maybe I'll complete this exercise later.

-- 2.23
forEach :: Monad m => (a -> m b) -> [a] -> m ()
forEach f []     = return ()
forEach f (x:xs) = do f x
                      forEach f xs

-- 2.24 n/a
-- 2.25 n/a
-- 2.26 n/a

-- Tree data type, for holding lists nested to arbitrary depth.
data Tree a = Leaf a | Branch [Tree a] deriving (Eq)

instance Show a => Show (Tree a) where
    show (Leaf x)    = show x
    show (Branch xs) = "(" ++ interior ++ ")"
        where interior = concat $ intersperse " " (map show xs)

-- 2.27
reverseTree :: Tree a -> Tree a
reverseTree (Leaf x)    = Leaf x
reverseTree (Branch xs) = Branch (reverse xs)

deepReverseTree :: Tree a -> Tree a
deepReverseTree (Leaf x)    = Leaf x
deepReverseTree (Branch xs) = Branch (reverse $ map deepReverseTree xs)

-- 2.28
fringe :: Tree a -> [a]
fringe (Leaf a)    = [a]
fringe (Branch ts) = concat (map fringe ts)

-- 2.29
-- a.
data Rod a = Rod { len :: a, struct :: Mobile a } deriving (Eq,Show)

data Mobile a = Weight a
              | Mobile { left :: Rod a, right ::  Rod a }
              deriving (Eq,Show)

-- b.
totalWeight :: Num a => Mobile a -> a
totalWeight (Weight x)   = x
totalWeight (Mobile l r) = totalWeight (struct l) + totalWeight (struct r)

-- c.
balanced :: Num a => Mobile a -> Bool
balanced (Weight x)   = True
balanced (Mobile l r) = torque l == torque r
                        && balanced (struct l)
                        && balanced (struct r)
    where torque (Rod len str) = len * totalWeight str

-- 2.30
squareTree :: Num a => Tree a -> Tree a
squareTree (Leaf x)    = Leaf (x^2)
squareTree (Branch xs) = Branch (square xs)
    where square []     = []
          square (y:ys) = (squareTree y): square ys

squareTree' :: Num a => Tree a -> Tree a
squareTree' (Leaf x)    = Leaf (x^2)
squareTree' (Branch xs) = Branch (map squareTree' xs)

-- 2.31
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x)    = Leaf (f x)
treeMap f (Branch xs) = Branch (map (treeMap f) xs)

squareTree'' :: Num a => Tree a -> Tree a
squareTree'' t = treeMap (^2) t

-- 2.32
-- To find the subsets of a set S, first pick an element x. Each subset either
-- contains x or not. Of the subsets that contain x, removing the element x
-- leaves one of the subsets that doesn't contain x. Therefore to generate
-- all the subsets of S, find the subsets that don't contain x, and then take
-- the union of these, with copies of themselves with x appended.
subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = let rest = subsets xs
                  in rest ++ (map (x:) rest)

-- 2.33
accumulate :: (a -> b -> b) -> b -> [a] -> b
accumulate op initial [] = initial
accumulate op initial (x:xs) = x `op` accumulate op initial xs

accumMap :: (a -> b) -> [a] -> [b]
accumMap f xs = accumulate (\x y -> (f x) : y) [] xs

accumAppend :: [a] -> [a] -> [a]
accumAppend xs ys = accumulate (:) ys xs

accumLength :: Integral b => [a] -> b
accumLength xs = accumulate (\x n -> n + 1) 0 xs

-- 2.34
hornerEval :: Num a => a -> [a] -> a
hornerEval x coefSequence =
    accumulate (\thisCoef higherTerms -> higherTerms * x + thisCoef)
               0
               coefSequence

-- 2.35
countLeaves :: Integral n => Tree a -> n
countLeaves (Leaf x)    = 1
countLeaves (Branch xs) = accumulate (\x n -> n + x) 0 (map countLeaves xs)

-- 2.36
-- Not appropriate for Haskell, since functions cannot have a variable number
-- of arguments. The equivalent thing would be to define accumulate2, accumulate3
-- etc, up to some sensible upper bound.

-- 2.37
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 op xs ys = accumulate (\(x,y) fs -> (x `op` y) : fs) [] (zip xs ys) 

dotProduct :: Num a => [a] -> [a] -> a
dotProduct u v = accumulate (+) 0 (map2 (*) u v)

matrixVectorProduct :: Num a => [[a]] -> [a] -> [a]
matrixVectorProduct a v = map (dotProduct v) a

transpose :: [[a]] -> [[a]]
transpose xs
    | empty (head xs) = []
    | otherwise       = heads : transpose tails
    where
        heads = map head xs
        tails = map tail xs
        empty [] = True
        empty xs = False

matrixMatrixProduct :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMatrixProduct a b = map (matrixVectorProduct cols) a
    where cols = transpose b

-- 2.38
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight op initial [] = initial
foldRight op initial (x:xs) = x `op` foldRight op initial xs

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft op initial xs = iter initial xs
    where iter result []     = result
          iter result (x:xs) = iter (result `op` x) xs

-- 2.39
reverseRight :: [a] -> [a]
reverseRight xs = foldRight (\x res -> res ++ [x]   ) [] xs

reverseLeft :: [a] -> [a]
reverseLeft xs = foldLeft (flip (:)) [] xs

-- 2.40 (uses isPrime from Chapter 1, Section 2)
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = smallestDivisor n == n
    where
        smallestDivisor n = findDivisor n 2
        findDivisor n test | test ^ 2 > n     = n
                           | test `divides` n = test
                           | otherwise        = findDivisor n (test + 1)
        divides a b = b `rem` a == 0

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = accumulate (++) [] (map f xs)

isPrimeSum :: Integral a => (a, a) -> Bool
isPrimeSum (x,y) = isPrime (x + y)

makePairSum :: Num a => (a,a) -> (a,a,a)
makePairSum (x,y) = (x, y, x + y)

uniquePairs :: (Num a, Enum a) => a -> [(a,a)]
uniquePairs n = flatMap (\i -> pairsBelow i) [1..n]
    where pairsBelow i = map (\j -> (i,j)) [1..i-1]

primeSumPairs :: Integral a => a -> [(a,a,a)]
primeSumPairs n = map makePairSum $ filter isPrimeSum $ uniquePairs n

-- 2.41
tripleSum :: (Ord a, Num a, Enum a) => a -> [(a,a,a)]
tripleSum s = filter unique $ map makeTriples $ filter sumLessThan $ uniquePairs s
    where makeTriples (x, y) = (x, y, s - x - y)
          sumLessThan (x, y) = x + y < s
          unique (x, y, z) = x > y && y > z

-- 2.42
queens :: Int -> [[Int]]
queens boardSize = queenCols boardSize where
    queenCols k = if k == 0
        then [emptyBoard]
        else filter (isSafe)
                    (flatMap
                        (\restOfQueens ->
                            map (\newRow -> adjoinPosition newRow k restOfQueens)
                                [1..boardSize])
                        (queenCols (k - 1)))
    isSafe positions = noRowAttacks positions && noDiagAttacks positions
        where noRowAttacks xs  = allUnique xs
              noDiagAttacks xs = allUnique (diags xs) && allUnique (diags2 xs)
              diags xs         = map2 (-) xs [1..]
              diags2 xs        = map2 (+) xs [1..]
              allUnique xs     = length (nub xs) == length xs
    adjoinPosition newRow k restOfQueens = newRow : restOfQueens
    emptyBoard = []

-- 2.43
-- This is an extended example that uses I/O and images. Defer to later.

