{-# LANGUAGE TypeSynonymInstances #-}

import Data.Ratio
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing, Down(..))
import Data.List (partition, tails, sortBy)
import Control.Monad (forM, filterM, forever, (>=>), when)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import qualified System.MIDI as MIDI
import qualified Data.Sequence as Seq
import qualified Control.Monad.Random as Rand

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


data PlayingNote = PlayingNote {
    pnMidiNote :: Int,
    pnChannel  :: Int,
    pnPitch    :: Double  -- relative to A440 (TODO: different keys)
}
    deriving (Show)

data PlayState = PlayState {
    psConnection :: MIDI.Connection,
    psPlayingNotes :: [PlayingNote],
    psFreeChannels :: Seq.Seq Int
}
    deriving (Show)

instance Show MIDI.Connection where
    show _ = "<MIDI.Connection>"

ratioEnergy :: Double -> Double -> Double
ratioEnergy spikeyness x = minimum 
    [ scale*denom + abs (fromIntegral (round (denom*x)) / denom - x) | denom <- [1..50]]
    where
    scale = 1/spikeyness

-- input should be sorted descending
energy :: Double -> [Double] -> Double
energy spikeyness notes = (spikeyness *) . sum $ do
    (x:xs) <- tails notes
    y <- xs
    return (ratioEnergy spikeyness (x/y))
    
type Rand = Rand.Rand Rand.StdGen

perturb :: Double -> [Double] -> Rand [Double]
perturb delta = mapM (\x -> (x *) . (2 **) <$> Rand.getRandomR (-delta, delta))

_SPIKEYNESS = 10
_DELTA = 0.05

reduce1 :: Double -> [Double] -> Rand [Double]
reduce1 delta xs = do
    xs' <- perturb delta xs
    return $ if energy _SPIKEYNESS xs' < energy _SPIKEYNESS xs then xs' else xs

reduceN :: Double -> Int -> [Double] -> Rand [Double]
reduceN delta n = foldr (>=>) return (replicate n (reduce1 delta))

repitchState :: ([Double] -> [Double]) -> StateT PlayState IO ()
repitchState pf = do
    state <- get
    let notes = sortBy (comparing (Down . pnPitch)) (psPlayingNotes state)
    newNotes <- forM (zip notes (pf (map pnPitch notes))) $ \(note, newpitch) -> do
        let note' = note { pnPitch = newpitch }
        when (newpitch /= pnPitch note) $ sendPitch note'
        return note'
    put (state { psPlayingNotes = newNotes })

reduceStateN :: Int -> StateT PlayState IO ()
reduceStateN n = do
    gen <- lift Rand.newStdGen
    repitchState (\ps -> Rand.evalRand (reduceN _DELTA n ps) gen)
        
sendPitch :: PlayingNote -> StateT PlayState IO ()
sendPitch pn = do
    state <- get
    let note = 12 * logBase 2 (pnPitch pn) + 69
    let pitchBend = round (8191 * (note - fromIntegral (pnMidiNote pn)))
    liftIO $ MIDI.send (psConnection state) $ 
        MIDI.MidiMessage (pnChannel pn) (MIDI.PitchWheel pitchBend)

noteOn :: Double -> Int -> StateT PlayState IO ()
noteOn pitch vel = do
    let note = 12 * logBase 2 pitch + 69
    let midiNote = round note
    state <- get
    let channel Seq.:< channels = Seq.viewl (psFreeChannels state) -- TODO handle no free channels
    
    let playingNote = PlayingNote {
            pnMidiNote = midiNote,
            pnChannel = channel,
            pnPitch = pitch }

    liftIO $ MIDI.send (psConnection state) $ MIDI.MidiMessage channel (MIDI.NoteOn midiNote vel)
    sendPitch playingNote

    put $ state {
        psPlayingNotes = playingNote : psPlayingNotes state,
        psFreeChannels = channels
    }

noteOnKey :: Int -> Int -> StateT PlayState IO ()
noteOnKey key vel = noteOn (2**((fromIntegral key - 69)/12)) vel

noteOffKey :: Int -> StateT PlayState IO ()
noteOffKey note = do
    state <- get
    let (off,remain) = partition ((== note) . pnMidiNote) (psPlayingNotes state)
    channels <- forM off $ \pn -> do
        liftIO $ MIDI.send (psConnection state) $
            MIDI.MidiMessage (pnChannel pn) (MIDI.NoteOn (pnMidiNote pn) 0)
        return (pnChannel pn)
    put $ state { psPlayingNotes = remain,
                  psFreeChannels = psFreeChannels state Seq.>< Seq.fromList channels }
    
    

connectOutput :: String -> IO PlayState
connectOutput destName = do
    destinations <- MIDI.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> MIDI.getName d) destinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< MIDI.getName destination
    return $ PlayState {
        psConnection = conn,
        psPlayingNotes = [],
        psFreeChannels = Seq.fromList [1..8]
    }

connectInput :: String -> IO MIDI.Connection
connectInput sourceName = do
    sources <- MIDI.enumerateSources
    validSources <- filterM (\s -> (sourceName ==) <$> MIDI.getName s) sources
    putStrLn $ "There are " ++ show (length validSources) ++ " valid sources"
    let [source] = validSources
    conn <- MIDI.openSource source Nothing
    putStrLn . ("Connected to source " ++) =<< MIDI.getName source
    return conn

main :: IO ()
main = do
    state <- connectOutput "IAC Bus 1"
    source <- connectInput "UM-ONE"
    MIDI.start source
    (`evalStateT` state) . forever $ do
        liftIO $ threadDelay 1000  -- 1 millisec
        events <- liftIO (MIDI.getEvents source)
        mapM_ procEvent events
        reduceStateN 1

procEvent :: MIDI.MidiEvent -> StateT PlayState IO ()
procEvent (MIDI.MidiEvent _ (MIDI.MidiMessage _ msg)) = go msg
    where
    go (MIDI.NoteOn key vel) = noteOnKey key vel
    go (MIDI.NoteOff key _) = noteOffKey key
    go _ = return ()
