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

-- chordRoot [5/4,3/2]
-- 5/4 = m*r
-- 3/2 = n*r
--
-- suppose r = a/b.  multiply by b
--
-- 5/4*b = m*a
-- 3/2*b = n*a
-- 5*b = 4*m*a
-- 3*b = 2*n*a
--
--
-- 5 = 4*m*r
-- 3 = 2*n*r
--
-- 

-- input is ascending
chordRoot :: [Rational] -> WS.T W Rational
chordRoot [] = empty
chordRoot (r1:rs) = do
    posn' <- wChoice [ pure (posn % n)  | n <- [1..] ]
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

-- The chord roots proceed as r1/(posn/n) = (r1/posn)*n.  
-- The harmonics proceed as m*root.
-- n and m are both arbitrary, so we're really redoing work here.
-- We should just find the appropriate i for (r1/posn)*i.

harmonizeRange :: Rational -> Rational -> [Rational] -> WS.T W Rational
harmonizeRange lo hi [] = empty
harmonizeRange lo hi (r1:rs) = 
    wChoice . map pure . nub . catMaybes . map range . takeWhile (<= hi) $ [ s*n | n <- [1..] ]
    where
    ratios = map (/r1) rs
    posn = fromIntegral (lcms (map denominator ratios))
    s = r1/posn
    range = listToMaybe . takeWhile (<= hi) . dropWhile (<= lo) . iterate (2*)

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
    amp = 1/8
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
    mapM (withHarmonies (1/2) 1) (parallel minorScale)

