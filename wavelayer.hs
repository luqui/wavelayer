import Data.Ratio
import qualified Csound.Base as Cs
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


type W = Integer
type Interval = Rational

-- normalizes an interval into the octave [1,2)
normalizeInterval :: Interval -> Interval
normalizeInterval i
    | i < 0 = normalizeInterval (negate i)
    | i < 1 = normalizeInterval (2*i)
    | i >= 2 = normalizeInterval (i/2)
    | otherwise = i


harmonicOctaveIntervals :: Integer -> [(Rational,Interval)]
harmonicOctaveIntervals octave = [ (a%(2^octave), b%a)
                                 | a <- [2^octave..2*2^octave], b <- [a..2*2^octave] ] 

-- gives base note, interval, and octave
harmonicOctaves :: WS.T W [(Integer,Rational,Interval)]
harmonicOctaves = wChoice [ return (map (\(root,i) -> (octave,root,i)) (harmonicOctaveIntervals octave)) 
                          | octave <- [0..] ] 

intervals :: WS.T W Interval
intervals = do
    oct <- harmonicOctaves
    oneOf [ i | (_,_,i) <- oct ]

harmonics :: WS.T W [Interval]
harmonics = 
    wChoice [ return [ a%(2^octave) | a <- [2^octave..2*2^octave-1] ] | octave <- [0..] ]

subset :: (Ord a) => [a] -> [a] -> Bool
subset xs ys = Set.fromList xs `Set.isSubsetOf` Set.fromList ys

-- takes a list of ascending notes and finds their rank and root
harmonicRoot :: [Rational] -> WS.T W (Integer,Rational)
harmonicRoot [] = empty
harmonicRoot [x] = pure (0,x)  -- or maybe empty?
harmonicRoot notes = do
    oct <- harmonicOctaves
    let octIntervals = Set.fromList $ map (\(_,_,i) -> i) oct
    guard (intervals `Set.isSubsetOf` octIntervals)
    oneOf $ nub [ (o,n1/root) | (o,root,i) <- oct, i == n2/n1 ]
    where
    intervals = Set.fromList [ normalizeInterval (b/a) | (a:as) <- tails notes, b <- as ]
    (n1:n2:_) = notes

harmonicRoot' :: [Rational] -> WS.T W Rational
harmonicRoot' rs = do
    h <- harmonics
    oneOf [ r | r <- rs, rs `subset` map (normalizeInterval . (r*)) h ]
    

harmonize :: [Rational] -> WS.T W Rational
harmonize rs = do
    root <- harmonicRoot' rs
    n <- (root*) <$> join (oneOf <$> harmonics)
    guard (n `notElem` rs)
    return n
    

harmonize1 :: [Rational] -> [Rational]
harmonize1 rs = sort $ head (WS.toList (harmonize rs)) : rs

iter :: Integer -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = f . iter (n-1) f

chord rs = sum [ amp * Cs.osc (440*realToFrac f) | f <- rs ]
    where
    amp = 0.25 / fromIntegral (length rs)
