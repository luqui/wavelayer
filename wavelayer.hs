{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Ratio
import qualified Csound.Base as Cs
import Csound.Control.Evt as Cs
import qualified Data.Set as Set
import qualified Control.Monad.WeightedSearch as WS
import Control.Applicative
import Data.List (tails, sort)
import Control.Monad (guard, join)
import Control.Arrow (second)


nub :: (Ord a) => [a] -> [a]
nub = go Set.empty
    where
    go _ [] = []
    go s (x:xs) | x `Set.member` s = go s xs
                | otherwise = x : go (Set.insert x s) xs

choice :: (Alternative f) => [f a] -> f a
choice = foldr (<|>) empty

oneOf :: (Alternative f) => [a] -> f a
oneOf = choice . map pure

wChoice :: (Num n, WS.Weight n) => [WS.T n a] -> WS.T n a
wChoice [] = empty
wChoice (x:xs) = x <|> WS.weight 1 (wChoice xs)

minimaOn :: (Ord b) => (a -> b) -> [a] -> [a]
minimaOn measure [] = []
minimaOn measure (x:xs) = go [] (measure x) xs
    where
    go ms mf [] = ms
    go ms mf (x:xs) =
        case compare fx mf of
            LT -> go [x] fx xs
            EQ -> go (x:ms) mf xs
            GT -> go ms mf xs
        where
        fx = measure x

lcms :: (Integral a) => [a] -> a
lcms = foldr lcm 1

iter :: Integer -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = f . iter (n-1) f

iterM :: (Monad m) => Integer -> (a -> m a) -> (a -> m a)
iterM 0 _ x = return x
iterM n f x = f =<< iterM (n-1) f x

inRange :: (Ord a) => a -> a -> a -> Bool
inRange lo hi x = lo <= x && x <= hi


type W = Integer


-- input is ascending
chordRoot :: [Rational] -> WS.T W Rational
chordRoot [] = empty
chordRoot (r1:rs) = do
    posn' <- wChoice [ pure n | n <- [1..] ]
    return $ r1 / posn'
    where
    ratios = map (/r1) rs
    posn = lcms (map denominator ratios)

harmonize :: [Rational] -> WS.T W Rational
harmonize rs = do
    root <- chordRoot rs
    harm <- wChoice [ pure (n*root) | n <- [1..] ]
    guard (harm `notElem` rs)
    return $ harm


type D = Cs.D
type D8 = (D,D,D,D,D,D,D,D)

chordSig :: [Rational] -> D8
chordSig rs = (el 0, el 1, el 2, el 3, el 4, el 5, el 6, el 7)
    where
    el n | n < len = realToFrac (rs !! n)
         | otherwise = 0
    len = length rs

chord :: [Rational] -> Cs.Sig
chord rs = sum [ amp * Cs.osc (440*realToFrac f) | f <- rs ]
    where
    amp = 0.25 / fromIntegral (length rs)

playChord :: D8 -> Cs.Sig
playChord (a,b,c,d,e,f,g,h) = amp*o a + amp*o b + amp*o c + amp*o d + amp*o e + amp*o f + amp*o g + amp*o h
    where
    amp = 1/8
    o x = Cs.osc (Cs.sig (440*x))

playChords :: [[Rational]] -> Cs.Sig
playChords rs = Cs.sched (return . playChord) (Cs.withDur 1 (Cs.cycleE (map chordSig rs) (Cs.metroE 1)))
