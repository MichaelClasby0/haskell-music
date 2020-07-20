module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List
      
type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
        
outputFilePath :: FilePath
outputFilePath = "output.bin"
                         
volume :: Float
volume = 1

sampleRate :: Samples
sampleRate = 48000.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

attackDurationPercent :: Float
attackDurationPercent = 0.01

decayDurationPercent :: Float
decayDurationPercent = 0.5

sustainLevelPercent :: Float
sustainLevelPercent = 0.5

releaseDurationPercent :: Float
releaseDurationPercent = 0.2

-- The A above the middle C (A_4)
pitchStandard :: Hz
pitchStandard = 440.0

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) duration
  where
    duration :: Seconds
    duration = beats * beatDuration

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith4 (\w x y z -> w * x * y * z) attack decay release output
  where
    step = (hz * 2 * pi) / sampleRate
    samples = sampleRate * duration

    attackLength = attackDurationPercent * samples -- No of samples attack should last for
    attackStep = 1/attackLength
    attack :: [Pulse]
    attack = map (min 1.0) [0.0, attackStep ..]

    decayLength = decayDurationPercent * samples -- No of samples decay should last for
    decayStep = (1 - sustainLevelPercent)/decayLength
    decay :: [Pulse]
    decay = concat [take (round attackLength) (repeat 1.0),
                    [1, 1 - decayStep .. sustainLevelPercent],
                    repeat sustainLevelPercent]
    
    releaseLength = releaseDurationPercent * samples -- No of samples release should last for
    releaseStep = sustainLevelPercent/releaseLength
    release :: [Pulse]
    release = reverse $ take (length output) (map (min 1) [0.0, releaseStep ..])
    
    output :: [Pulse]
    output = map (sin . (*) step) [0.0 .. samples]
    
-- Darude - Sandstorm
wave :: [Pulse]
wave =
  concat
    [ note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.25
    , note 5 0.5
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.25
    , note 3 0.5
    , note (-2) 0.5
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.25
    , note 0 0.5
    ]


-- Megalovania
hehehe :: [Pulse]
hehehe =
  concat [ note 0 0.25
         , note 0 0.25
         , note 12 0.5
         , note 7 (0.5 + 0.25)
         , note 6 0.5
         , note 5 0.5
         , note 3 0.5
         , note 0 0.25
         , note 3 0.25
         , note 5 0.25
         ]

test :: [Pulse]
test =
  concat [ note 0 1
         , note 1 1
         , note 2 1
         , note 3 1
         , note 4 1
         , note 5 1
         , note 6 1
         , note 7 1
         , note 8 1
         , note 9 1
         , note 10 1
         , note 11 1
         , note 0 1
         , note (-2) 1
         , note (-4) 1
         ]


save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
