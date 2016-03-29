import Data.Ratio

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


