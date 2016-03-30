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

calcPitch :: [Rational] -> MIDINote -> Rational
calcPitch [] n = realToFrac (equalTempPitch n)  -- should incorporate history
calcPitch (root:rest) n = minimumBy (comparing measure) (take 100 candidates)
    where
    pitch = equalTempPitch n
    measure p = maximum (1 : [ numerator (p/q) + denominator (p/q) | q <- rest ])
    candidates = map (root*) $ rationalApproxs (pitch / delta / realToFrac root) (pitch * delta / realToFrac root)
    delta = 2**(1/24)

calcChord :: [MIDINote] -> [Rational]
calcChord = go []
    where
    go accum [] = []
    go accum (n:ns) = let p = calcPitch accum n in p : go (p:accum) ns

main = do
    sources <- MIDI.enumerateSources
    print =<< mapM MIDI.getName sources
