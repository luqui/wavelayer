import Data.Ratio
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Control.Applicative
import qualified System.MIDI as MIDI

mergeOn :: (Ord b) => (a -> b) -> [a] -> [a] -> [a]
mergeOn _ [] ys = ys
mergeOn _ xs [] = xs
mergeOn measure (x:xs) (y:ys)
    | measure x <= measure y = x : mergeOn measure xs (y:ys)
    | otherwise              = y : mergeOn measure (x:xs) ys

rationalApproxs :: Double -> Double -> [Rational]
rationalApproxs lower upper = go (floor lower % 1) (ceiling upper % 1)
    where
    go l u | realToFrac u <= lower = []
           | realToFrac l >= upper = []
           | otherwise  = (if inRange mediant then (mediant:) else id) $ 
                            mergeOn denominator (go l mediant) (go mediant u)
        where
        mediant = (numerator l + numerator u) % (denominator l + denominator u)
        inRange x = lower < realToFrac x && realToFrac x < upper



type MIDINote = Int

equalTempPitch :: MIDINote -> Double
equalTempPitch n = 440 * 2 ** (fromIntegral (n - 69) / 12)

-- Takes a list of ratios already present, and a number of half-steps from 1, and calculates
-- the most consonant ratio.
calcPitch :: [Rational] -> Int -> Rational
calcPitch accum n = leftBiasedMinimumOn measure (take 30 candidates)
    where
    pitch = 2 ** (fromIntegral n / 12)
    measure p = maximum (0 : [ numerator (p/q) + denominator (p/q) | q <- accum ])
    candidates = rationalApproxs (pitch / delta) (pitch * delta)
    delta = 2**(1/24)

leftBiasedMinimumOn :: (Ord b) => (a -> b) -> [a] -> a
leftBiasedMinimumOn measure [] = error "leftBiasedMinimumOn: empty list"
leftBiasedMinimumOn measure (x:xs) = go x (measure x) xs
    where
    go best bestm [] = best
    go best bestm (x:xs)
        | m < bestm = go x m xs
        | otherwise = go best bestm xs
        where
        m = measure x

calcChord :: [Int] -> [Rational]
calcChord = go []
    where
    go accum [] = []
    go accum (n:ns) = let p = calcPitch accum n in p : go (p:accum) ns

ratios :: [Rational] -> [Rational]
ratios [] = []
ratios (p:ps) = 1 : map (/p) ps

main = do
    sources <- MIDI.enumerateSources
    print =<< mapM MIDI.getName sources
