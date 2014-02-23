{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Heap 
    ( MonadDelay(..)
    , weight'
    , T
    , Heap, flattenHeap
    , Heap', flattenHeap'
    , Weight(..), weight
    , chopHeap, chopHeapN
    , chop, chopN
    )
where

import MonadDelay
import Control.Applicative
import Control.Monad (ap, liftM, MonadPlus(..))

data Heap w a
    = Fail
    | Yield a (Heap w a)
    | Weight w (Heap w a)
    deriving (Show, Functor)


chopHeap :: (ZeroWeight w) => w -> Heap w a -> Heap w a
chopHeap w _ | w <= zeroWeight = Fail
chopHeap _ Fail = Fail
chopHeap w (Yield a h) = Yield a (chopHeap w h)
chopHeap w (Weight w' h) = Weight w' (chopHeap (w `difference` w') h)

chopHeapN :: Int -> Heap w a -> Heap w a
chopHeapN 0 _ = Fail
chopHeapN _ Fail = Fail
chopHeapN n (Yield a h) = Yield a (chopHeapN (n-1) h)
chopHeapN n (Weight w h) = Weight w (chopHeapN n h)

newtype Heap' w a = Heap' { getHeap' :: forall r. (a -> Heap w r) -> Heap w r }

-- these don't work because CPS
chop :: (ZeroWeight w) => w -> Heap' w a -> Heap' w a
chop w h = Heap' $ \r -> chopHeap w (getHeap' h r)

chopN :: Int -> Heap' w a -> Heap' w a
chopN n h = Heap' $ \r -> getHeap' h (chopHeapN n . r)

instance Monad (Heap' w) where
    return x = Heap' ($ x)
    m >>= f = Heap' $ \r -> getHeap' m (\x -> getHeap' (f x) r)

instance Functor (Heap' w) where
    fmap = liftM

instance Applicative (Heap' w) where
    pure = return
    (<*>) = ap

-- This monadplus instance suffers from the foldr merge problem
-- We can do better by using a tree-merge of some sort.
instance (Weight w) => MonadPlus (Heap' w) where
    mzero = Heap' $ const Fail
    m `mplus` n = Heap' $ \r -> getHeap' m r `mplus` getHeap' n r

instance (Weight w) => Alternative (Heap' w) where
    empty = mzero
    (<|>) = mplus

instance (UnitWeight w) => MonadDelay (Heap' w) where
    delay m = Heap' $ \r -> Weight unitWeight (getHeap' m r)

weight = Weight
weight' w m = Heap' $ \r -> Weight w (getHeap' m r)

class (Ord w) => Weight w where
    difference :: w -> w -> w

class (Weight w) => ZeroWeight w where
    zeroWeight :: w

class (Weight w) => UnitWeight w where
    unitWeight :: w

instance Weight Int where
    difference = (-)

instance UnitWeight Int where
    unitWeight = 1

instance ZeroWeight Int where
    zeroWeight = 0


instance Weight Double where
    difference = (-)

instance UnitWeight Double where
    unitWeight = 1

instance ZeroWeight Double where
    zeroWeight = 0


instance (Weight w) => Monad (Heap w) where
    return x = Yield x Fail
    Fail >>= _ = Fail
    Yield x m >>= f = f x `mplus` (m >>= f)
    Weight w m >>= f = Weight w (m >>= f)

instance (Weight w) => MonadPlus (Heap w) where
    mzero = Fail
    Fail `mplus` m = m
    Yield x m `mplus` n = Yield x (m `mplus` n)
    Weight w m `mplus` Fail = Weight w m
    Weight w m `mplus` Yield x n = Yield x (Weight w m `mplus` n)
    Weight w m `mplus` Weight w' n
        = case compare w w' of
            LT -> Weight w (m `mplus` Weight (difference w' w) n)
            EQ -> Weight w (m `mplus` n)
            GT -> Weight w' (Weight (difference w w') m `mplus` n)

instance (Weight w) => Alternative (Heap w) where
    empty = mzero
    (<|>) = mplus

instance (Weight w) => Applicative (Heap w) where
    pure = return
    (<*>) = ap

instance (UnitWeight w) => MonadDelay (Heap w) where
    delay = Weight unitWeight
    

flattenHeap :: Heap w a -> [a]
flattenHeap Fail = []
flattenHeap (Yield x hs) = x : flattenHeap hs
flattenHeap (Weight w hs) = flattenHeap hs

flattenHeap' :: Heap' w a -> [a]
flattenHeap' h = flattenHeap (getHeap' h (\x -> Yield x Fail))


type T = Heap'
