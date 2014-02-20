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
import Data.Maybe (listToMaybe, catMaybes)


nub :: (Ord a) => [a] -> [a]
nub = go Set.empty
    where
    go _ [] = []
    go s (x:xs) | x `Set.member` s = go s xs
                | otherwise = x : go (Set.insert x s) xs

choice :: (Alternative f) => [f a] -> f a
choice = foldr (<|>) empty

wChoice :: (Num n, WS.Weight n) => [WS.T n a] -> WS.T n a
wChoice [] = empty
wChoice (x:xs) = x <|> WS.weight 1 (wChoice xs)

lcms :: (Integral a) => [a] -> a
lcms = foldr lcm 1

iterM :: (Monad m) => Integer -> (a -> m a) -> (a -> m a)
iterM 0 _ x = return x
iterM n f x = f =<< iterM (n-1) f x


type W = Integer

harmonizeRange :: Rational -> Rational -> [Rational] -> WS.T W Rational
harmonizeRange lo hi [] = empty
harmonizeRange lo hi rs@(r1:rs') = 
    wChoice $ do
        s' <- [s/n | n <- [1..]]
        return . choice . map pure . nub . catMaybes . map range . takeWhile (<= hi) $ [ s'*n | n <- [1..] ]
    where
    ratios = map (/r1) rs'
    posn = fromIntegral (lcms (map denominator ratios))
    s = r1/posn
    range = listToMaybe . filter (`notElem` rs) . 
        (takeWhile (<= hi) . dropWhile (<= lo) . iterate (*2)) 
    -- iterate (*2) advances earlier harmonics to higher frequencies, to pick "simpler"
    -- harmonics first.

withHarmonies :: Rational -> Rational -> [Rational] -> WS.T W [Rational]
withHarmonies lo hi rs = do
    r <- harmonizeRange lo hi rs
    return . sort $ r : rs


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
    amp = 1/16
    o x = Cs.osc (Cs.sig (440*x))

playChords :: [[Rational]] -> Cs.Sig
playChords rs = Cs.sched (return . playChord) (Cs.withDur 1 (Cs.cycleE (map chordSig rs) (Cs.metroE 1)))


majorScale :: [Rational]
majorScale = [1,9/8,5/4,4/3,3/2,10/6,15/8]

minorScale :: [Rational]
minorScale = [1,9/8,6/5,4/3,3/2,8/5,16/9]  -- 2-(5+5) 7th, 5+5 2nd
minorScale' :: [Rational]
minorScale' = [1,9/8,6/5,4/3,3/2,8/5,9/5]  -- m3+5 7th, 5+5 2nd
minorScale'' :: [Rational]
minorScale'' = [1,10/9,6/5,4/3,3/2,8/5,9/5] -- m3+5 7th, 4-m3 2nd
minorScale''' :: [Rational]
minorScale''' = [1,10/9,6/5,4/3,3/2,8/5,16/9] -- 2-(5+5) 7th, 4-m3 2nd

parallel :: [Rational] -> [[Rational]]
parallel scale = zipWith (\a b -> [a,b]) scale (drop 2 (scale ++ map (2*) scale))

harmscale :: WS.T W [[Rational]]
harmscale = do
    mapM (iterM 2 (withHarmonies (1/2) 1)) (parallel majorScale)


main = Cs.dac . playChords . head . WS.toList $ harmscale
