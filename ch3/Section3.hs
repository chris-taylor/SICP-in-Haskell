{-# LANGUAGE ExistentialQuantification #-}

-- Modeling with Mutable Data

import Control.Monad.ST (runST, ST)
import Control.Monad (liftM)
import Data.STRef
import Data.IORef

-- Haskell doesn't have a built-in immutable list type, unlike Scheme, so
-- we'll have to create one. This mutable list is backed by a regular
-- linked list full of STRefs.

data MutableList s a = MutableList [STRef s a]

fromList :: [a] -> ST s (MutableList s a)
fromList xs = liftM MutableList (mapM newSTRef xs)

toList :: MutableList s a -> ST s [a]
toList (MutableList xs) = mapM readSTRef xs

setCar :: MutableList s a -> a -> ST s (MutableList s a)
setCar (MutableList (_:xs)) y = do
    x <- newSTRef y
    return (MutableList (x:xs))

setCdr :: MutableList s a -> MutableList s a -> ST s (MutableList s a)
setCdr (MutableList (x:_)) (MutableList xs) = return (MutableList (x:xs))

testMutableList = runST $ do
    xs <- fromList [1,2,3]
    ys <- setCar xs 4
    zs <- fromList [5,6]
    ws <- setCdr ys zs
    toList ws