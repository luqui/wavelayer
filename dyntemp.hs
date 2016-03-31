{-# LANGUAGE TypeSynonymInstances #-}

import Data.Ratio
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.List (partition)
import Control.Monad (forM_)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans
import qualified System.MIDI as MIDI
import qualified Data.Sequence as Seq

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
    pnRatio    :: Rational  -- relative to A440 (TODO: different keys)
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


noteOn :: Rational -> Int -> StateT PlayState IO ()
noteOn pitch vel = do
    let note = 12 * logBase 2 (realToFrac pitch) + 69
    let midiNote = round note
    let pitchBend = round (8191 * (note - fromIntegral midiNote))
    state <- get
    let channel Seq.:< channels = Seq.viewl (psFreeChannels state) -- TODO handle no free channels
    
    liftIO $ MIDI.send (psConnection state) $ MIDI.MidiMessage channel (MIDI.NoteOn midiNote vel)
    liftIO $ MIDI.send (psConnection state) $ MIDI.MidiMessage channel (MIDI.PitchWheel pitchBend)

    let playingNote = PlayingNote {
            pnMidiNote = midiNote,
            pnChannel = channel,
            pnRatio = pitch }

    put $ state {
        psPlayingNotes = playingNote : psPlayingNotes state,
        psFreeChannels = channels
    }

noteOff :: Rational -> StateT PlayState IO ()
noteOff pitch = do
    state <- get
    let (off,remain) = partition ((== pitch) . pnRatio) (psPlayingNotes state)
    forM_ off $ \pn ->
        liftIO $ MIDI.send (psConnection state) $
            MIDI.MidiMessage (pnChannel pn) (MIDI.NoteOn (pnMidiNote pn) 0)
    put $ state { psPlayingNotes = remain }

noteOnKey :: Int -> Int -> StateT PlayState IO ()
noteOnKey note vel = do
    state <- get
    let ratios = map pnRatio (psPlayingNotes state)
    let newRatio = calcPitch ratios (note-69)
    noteOn newRatio vel

noteOffKey :: Int -> StateT PlayState IO ()
noteOffKey note = do
    state <- get
    let (off,remain) = partition ((== note) . pnMidiNote) (psPlayingNotes state)
    forM_ off $ \pn ->
        liftIO $ MIDI.send (psConnection state) $
            MIDI.MidiMessage (pnChannel pn) (MIDI.NoteOn (pnMidiNote pn) 0)
    put $ state { psPlayingNotes = remain }
    
    

connect :: IO PlayState
connect = do
    destination:_ <- MIDI.enumerateDestinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to " ++) =<< MIDI.getName destination
    return $ PlayState {
        psConnection = conn,
        psPlayingNotes = [],
        psFreeChannels = Seq.fromList [1..8]
    }

