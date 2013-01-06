-- 3.3 Modeling with Mutable Data

import Control.Monad.ST (runST, ST)
import Control.Monad (liftM)
import Data.STRef

-- Haskell doesn't have a built-in mutable list type, unlike Scheme, so
-- we'll have to create one. This mutable list is a linked list full of STRefs.

data List s a = Nil | Cons (STRef s a) (STRef s (List s a))

get :: STRef s a -> ST s a
get = readSTRef

set :: STRef s a -> a -> ST s ()
set = writeSTRef

var :: a -> ST s (STRef s a)
var = newSTRef

car :: List s a -> ST s a
car Nil        = error "car of an empty list"
car (Cons x _) = get x

cdr :: List s a -> ST s (List s a)
cdr Nil        = error "cdr of an empty list"
cdr (Cons _ x) = get x

setCar :: List s a -> a -> ST s ()
setCar lst a = case lst of
    Nil      -> error "setCar of an empty list"
    Cons x _ -> set x a

setCdr :: List s a -> List s a -> ST s ()
setCdr a lst = case lst of
    Nil      -> error "setCdr of an empty list"
    Cons _ x -> set x a

fromList :: [a] -> ST s (List s a)
fromList [] = return Nil
fromList (x:xs) = do
    y   <- var x
    ys  <- fromList xs >>= var
    return (Cons y ys)

toList :: List s a -> ST s [a]
toList Nil         = return []
toList (Cons x xs) = do
    y  <- get x
    ys <- get xs >>= toList
    return (y:ys)


-- ex 3.12

lastPair :: List s a -> ST s (List s a)
lastPair x = do
    x' <- cdr x
    case x' of
        Nil -> return x
        _   -> lastPair x'

append :: List s a -> List s a -> ST s (List s a)
append x y = case x of
    Nil       -> return y
    Cons x xs -> do
        xs' <- get xs
        ys' <- append xs' y >>= var
        return (Cons x ys')

append' :: List s a -> List s a -> ST s (List s a)
append' x y = lastPair x >>= setCdr y >> return x

test = runST $ do
    x <- fromList ['a','b']
    y <- fromList ['c','d']
    z <- append x y
    toList z
    cdr x >>= toList
    w <- append' x y
    toList w
    cdr x >>= toList

