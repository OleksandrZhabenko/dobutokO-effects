-- |
-- Module      :  DobutokO.Sound.Effects.Classes.SndParamSet
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module DobutokO.Sound.Effects.Classes.SndParamSet where

-- inspired with: https://wiki.haskell.org/Scoped_type_variables
import GHC.Base (asTypeOf)
import DobutokO.Sound.Effects.BassTreble
import DobutokO.Sound.Effects.Bend
import DobutokO.Sound.Effects.Biquad
import DobutokO.Sound.Effects.Chorus
import DobutokO.Sound.Effects.DCShift
import DobutokO.Sound.Effects.Dither
import DobutokO.Sound.Effects.Echo
import DobutokO.Sound.Effects.Fade
import DobutokO.Sound.Effects.FIR
import DobutokO.Sound.Effects.Flanger
import DobutokO.Sound.Effects.Gain
import DobutokO.Sound.Effects.LADSPA
import DobutokO.Sound.Effects.Loudness
import DobutokO.Sound.Effects.MCompand
import DobutokO.Sound.Effects.Noise
import DobutokO.Sound.Effects.Overdrive
import DobutokO.Sound.Effects.Pad
import DobutokO.Sound.Effects.PassReject
import DobutokO.Sound.Effects.Phaser
import DobutokO.Sound.Effects.Pitch
import DobutokO.Sound.Effects.Rate
import DobutokO.Sound.Effects.Remix
import DobutokO.Sound.Effects.Reverb
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Speed
import DobutokO.Sound.Effects.Splice
import DobutokO.Sound.Effects.Stats
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.Effects.Tremolo
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One

class SndParamSet a b where
  set2 :: a -> b
  set2G :: a -> b -> b
  set2G x = asTypeOf (set2 x)

class SndParamSet3 a b where
  set23 :: a -> b -> b
    
class SndParamSet3M a b where
  set23m :: a -> b -> Maybe b 
     
class SndParamSetL a b where
  set2L :: [a] -> b
  set2GL :: [a] -> b -> b
  set2GL xs = asTypeOf (set2L xs)

class SndParamSetL3 a b where
  set23L :: [a] -> b -> b

class SndParamSet3O a b where
  set23o :: (One2 a) -> b -> b

instance SndParamSet3 b (FreqWidthS a b) where
  set23 = freqWidthSSet2

instance SndParamSet3 b (Bass a b) where
  set23 = bassSet2

instance SndParamSet3 b (Treble a b) where
  set23 = trebleSet2

instance SndParamSet3 b (BendTrio a b) where
  set23 = bendTrioSet2

instance SndParamSet3 b (Bend a b c) where
  set23 = bendSet2

instance SndParamSet3 a (Coeffs a)  where
  set23 = coeffsSet2 

instance SndParamSet3 (Coeffs a) (Biquad a) where
  set23 = biquadSet2

instance SndParamSet3 a (ChorusTail a b)  where
  set23 = chorusTailSet1 2

instance SndParamSet3 a (Chorus a b)  where
  set23 = chorusSet1 2  

instance SndParamSet3 b (DCShift a b) where
  set23 = dcShiftSet2

instance SndParamSet3 b (Dither a b c) where 
  set23 = ditherSet2

instance SndParamSet3 a (EchoTail a)  where
  set23 = echoTailSet1 2 

instance SndParamSet3 a (Echo a b)  where
  set23 = echoSet1 2

instance SndParamSet3 a (Echos a b)  where
  set23 = echosSet1 2

instance SndParamSetL3 b (Fade2 a b) where
  set23L = fadeSet2

instance SndParamSet3 String Fade where
  set23 = fadeSet2E 2

instance SndParamSetL b (Fir a b) where
  set2L = firSet2

instance SndParamSet3 b (Flanger a b) where 
  set23 = flangerSet2

instance SndParamSet3 b (Gain1 a b c d) where
  set23 = gainSet2

instance SndParamSet3O b (Ladspa1 a b c) where
  set23o = ladspaSet2

instance SndParamSet3 a (Loudness a)  where
  set23 = loudnessSet2

instance SndParamSet3M b (CompandTail a b) where 
  set23m = compandTailSet2

instance SndParamSet3 a (Pair a)  where 
  set23 = pairSet2

instance SndParamSetL3 a (AtDe a)  where
  set23L = atDeSet2

instance SndParamSet3 b (Compand a b c d) where
  set23 = compandSet2

instance SndParamSet3 b (FreqComp a b) where
  set23 = freqCompSet2

instance SndParamSetL3 b (MCompand a b) where
  set23L = mCompandSet2

instance SndParamSet3M b (Noisered a b) where 
  set23m = noiseredSet2

instance SndParamSet3M a (Overdrive a)  where 
  set23m = overdriveSet2

instance SndParamSet3 a (PadSpec a)  where
  set23 = padSpecSet2

instance SndParamSetL3 b (Pad a b) where
  set23L = padSet2

instance SndParamSet3 b (FreqWidth a b) where
  set23 = freqWidthSet2

instance SndParamSet3 b (BandPass a b) where
  set23 = bandPassSet2

instance SndParamSet3 b (Band a b) where
  set23 = bandSet2

instance SndParamSet3 b (HighPass a b) where
  set23 = highPassSet2

instance SndParamSet3 b (LowPass a b) where
  set23 = lowPassSet2

instance SndParamSet3 b (Equalizer a b) where
  set23 = equalizerSet2

instance SndParamSet3 a (Phaser a b)  where
  set23 = phaserSet1 2

instance SndParamSet3 b (Pitch a b c) where 
  set23 = pitchSet2

instance SndParamSet3 b (RateL a b) where
  set23 = rateLSet2

instance SndParamSet3 b1 (RateH a b1 b2 b3 b4 b5 c) where
  set23 = rateHSet21

instance SndParamSet RateHigh Rate where
  set2 = rate2Set2

instance SndParamSet3 (Vol3 b) (IChannel a b) where
  set23 = ichannelSet2
    
instance SndParamSetL3 OChanF ReMix where
  set23L = remixSet2

instance SndParamSet3 b (Reverb a b c d) where 
  set23 = reverbSet2

instance SndParamSet3 Float ReverbE where
  set23 = reverbSet3E 2  

instance SndParamSet3 b (Duration a b) where
  set23 = durationSet2d

instance SndParamSet3 b (AboveTSpec1 a b c) where
  set23 = aboveTSpecSet2a

instance SndParamSet3 b (BelowTSpec1 a b c) where 
  set23 = belowTSpecSet2b

instance SndParamSet3 b (Silence a b c) where
  set23 = silenceSet2
  
instance SndParamSet3 b (Sinc a b c d) where 
  set23 = sincSet2
    
instance SndParamSetL3 b (Spectrogram3 a b c d e) where
  set23L = spectrogramSet32

instance SndParamSet3 b (Speed a b) where
  set23 = speedSet2

instance SndParamSetL3 (One3 b) (Splice2 a b) where
  set23L = splice2Set2

instance SndParamSetL3 b (Stats2 a b) where
  set23L = stats2Set2

instance SndParamSet3 a (StretchP a)  where
  set23 = stretchSet2

instance SndParamSet3 b (Stretch2 a b) where
  set23 = stretch2Set2

instance SndParamSet3 b (Tempo a b c d) where 
  set23 = tempoSet2

instance SndParamSetL3 b (TimeSpec a b) where
  set23L = timeSpecSet2

instance SndParamSet3 a (Tremolo a)  where
  set23 = tremoloSet2

instance SndParamSet3 b (Vol2 a b) where
  set23 = volSet2
  
