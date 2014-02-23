{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Ratio
import qualified Csound as Cs
import Csound.Control.Evt as Cs
import qualified Data.Set as Set
import qualified Heap as WS
import Control.Applicative
import Data.List (tails, sort, delete, insert)
import Control.Monad (guard, join, mfilter, (<=<))
import Control.Arrow (second)
import Data.Maybe (listToMaybe, catMaybes)


nub :: (Ord a) => [a] -> [a]
nub = go Set.empty
    where
    go _ [] = []
    go s (x:xs) | x `Set.member` s = go s xs
                | otherwise = x : go (Set.insert x s) xs

concatNub :: (Ord a) => [[a]] -> [[a]]
concatNub = go Set.empty
    where
    go _ [] = []
    go s ([]:xss) = []:go s xss
    go s ((x:xs):xss)
        | x `Set.member` s = go s (xs:xss)
        | otherwise = let ys:yss = go (Set.insert x s) (xs:xss) in
                      (x:ys):yss

choice :: (Alternative f) => [f a] -> f a
choice = foldr (<|>) empty

choice' :: (Alternative f) => [a] -> f a
choice' = choice . map pure

wChoice :: (Num n, WS.Weight n) => [WS.Heap n a] -> WS.Heap n a
wChoice [] = empty
wChoice (x:xs) = x <|> WS.weight 1 (wChoice xs)

wChoice' :: (Num n, WS.Weight n) => [a] -> WS.Heap n a
wChoice' = wChoice . map pure

lcms :: (Integral a) => [a] -> a
lcms = foldr lcm 1

iterM :: (Monad m) => Integer -> (a -> m a) -> (a -> m a)
iterM 0 _ x = return x
iterM n f x = f =<< iterM (n-1) f x


type W = Double


harmonizeMP :: (Rational -> Bool) -> (Rational -> Bool) -> [Rational] -> WS.Heap W Rational
harmonizeMP lo hi [] = empty
harmonizeMP lo hi rs@(r1:rs') = do
    s' <- wChoice' [s/n | n <- [1..]]
    mfilter (liftA2 (&&) lo (`notElem` rs)) 
        . wChoice' 
        . takeWhile hi $ [ s'*n | n <- [1..] ]
    where
    ratios = map (/r1) rs'
    posn = fromIntegral (lcms (map denominator ratios))
    s = r1/posn

harmonizeRange :: Rational -> Rational -> [Rational] -> WS.Heap W Rational
harmonizeRange lo hi = harmonizeMP (>= lo) (<= hi) 

-- human range of hearing is 20 - 20000 Hz
harmonize :: [Rational] -> WS.Heap W Rational
harmonize = harmonizeRange 20 20000

withHarmonies :: Rational -> Rational -> [Rational] -> WS.Heap W [Rational]
withHarmonies lo hi rs = do
    r <- harmonizeRange lo hi rs
    return . sort $ r : rs


type D = Cs.D

chord :: [Rational] -> Cs.Sig
chord rs = sum [ amp * Cs.osc (realToFrac f) | f <- rs ]
    where
    amp = 0.25 / fromIntegral (length rs)

playNote :: D -> Cs.Sig
playNote n = env * Cs.osc (Cs.sig n)
  where
  env = Cs.linseg [1, 3, 0] * Cs.linseg [1, Cs.idur-0.06, 1, 0.05, 0]

playChords :: [[Rational]] -> Cs.Sig
playChords = (/8) . Cs.mix . Cs.sco (return . playNote) . fmap realToFrac . scoreChords


majorScale :: [Rational]
majorScale = [1,9/8,5/4,4/3,3/2,5/3,15/8]

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

harmscale :: [[Rational]]
harmscale = map (harmonizeN 4 (1/2) 2) (parallel minorScale)

harmonizeN :: Integer -> Rational -> Rational -> [Rational] -> [Rational]
harmonizeN n lo hi = head . WS.flattenHeap . iterM n (withHarmonies lo hi)


harmonizeLine :: [[Rational]] -> WS.Heap W [Rational]
harmonizeLine [] = return []
harmonizeLine (chord:chords) = do
    firstNote <- WS.chopHeap 10 $ harmonize chord
    (firstNote:) <$> harmonizeLineFrom firstNote chords

harmonizeLineFrom :: Rational -> [[Rational]] -> WS.Heap W [Rational]
harmonizeLineFrom prevNote [] = return []
harmonizeLineFrom prevNote (chord:chords) = do   
    nextNote <- WS.chopHeap 10 $ harmonize chord
    () <- WS.weight (30*abs (logBase 2 (realToFrac (nextNote/prevNote)))) (return ())
    (nextNote:) <$> harmonizeLineFrom nextNote chords

harmonizeIntoLine :: [[Rational]] -> WS.Heap W [[Rational]]
harmonizeIntoLine rs = do
    h <- harmonizeLine rs
    return $ zipWith insert h rs
        

--myLine = map (:[]) . map (*440) $ [1, 9/8, 4/3, 5/4, 5/4, 1, 9/8, 3/2, 8/5, 4/3, 3/2, 2, 15/8, 3/2, 4/3, 4/3, 5/4]

--myLine = map (:[]) . map (*440) $ [1, 9/8, 4/3, 5/4, 5/4, 1, 9/8, 3/2, 8/5, 4/3, 3/2]
myLine = map (:[]) . map (*440) $ [1, 9/8, 4/3, 5/4, 5/4, 1, 9/8, 3/2, 8/5, 4/3, 3/2, 2, 15/8, 3/2, 4/3, 4/3, 5/4]


-- This picks 4-part, 2-octave harmonies pivoting around A880, in increasing order of 
-- complexity (as defined by harmonizeRange).
chords :: [[Rational]]
chords = take 200 . nub . map (delete 2) . WS.flattenHeap $ iterM 4 (withHarmonies (1/2) 2) [2]

scoreChords :: [[Rational]] -> Cs.Score Rational
scoreChords = Cs.mel . map Cs.har . (map.map) Cs.temp

--main = Cs.dac . playChords . (map.map) (*440) $ chords

play = Cs.dac . playChords


cheapest = head . WS.flattenHeap

main = play . cheapest . harmonizeIntoLine $ myLine
