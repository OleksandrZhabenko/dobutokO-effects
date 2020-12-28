-- |
-- Module      :  DobutokO.Sound.Effects.Classes.FstParamSet
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

module DobutokO.Sound.Effects.Classes.FstParamSet where

-- inspired with: https://wiki.haskell.org/Scoped_type_variables
import GHC.Base (asTypeOf)
import DobutokO.Sound.Effects.BassTreble
import DobutokO.Sound.Effects.Bend
import DobutokO.Sound.Effects.Biquad
import DobutokO.Sound.Effects.Channels
import DobutokO.Sound.Effects.Chorus
import DobutokO.Sound.Effects.Contrast
import DobutokO.Sound.Effects.DCShift
import DobutokO.Sound.Effects.Delay
import DobutokO.Sound.Effects.Dither
import DobutokO.Sound.Effects.Downsample
import DobutokO.Sound.Effects.Echo
import DobutokO.Sound.Effects.Fade
import DobutokO.Sound.Effects.FIR
import DobutokO.Sound.Effects.Flanger
import DobutokO.Sound.Effects.Gain
import DobutokO.Sound.Effects.Hilbert
import DobutokO.Sound.Effects.LADSPA
import DobutokO.Sound.Effects.Loudness
import DobutokO.Sound.Effects.MCompand
import DobutokO.Sound.Effects.Misc
import DobutokO.Sound.Effects.Noise
import DobutokO.Sound.Effects.Overdrive
import DobutokO.Sound.Effects.Pad
import DobutokO.Sound.Effects.PassReject
import DobutokO.Sound.Effects.Phaser
import DobutokO.Sound.Effects.Pitch
import DobutokO.Sound.Effects.Rate
import DobutokO.Sound.Effects.Remix
import DobutokO.Sound.Effects.Repeat
import DobutokO.Sound.Effects.Reverb
import DobutokO.Sound.Effects.Segment
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Specs
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Speed
import DobutokO.Sound.Effects.Splice
import DobutokO.Sound.Effects.Stat
import DobutokO.Sound.Effects.Stats
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.Effects.Tremolo
import DobutokO.Sound.Effects.Trim
import DobutokO.Sound.Effects.Upsample
import DobutokO.Sound.Effects.Vad
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One

class FstParamSet a b where
  set1 :: a -> b
  set1G :: a -> b -> b
  set1G x = asTypeOf (set1 x)

class FstParamSet3 a b where
  set13 :: a -> b -> b
    
class FstParamSetL a b where
  set1L :: [a] -> b
  set1GL :: [a] -> b -> b
  set1GL xs = asTypeOf (set1L xs)

class FstParamSetL3 a b where
  set13L :: [a] -> b -> b

class FstParamSet3O a b where
  set1o :: (One2 a) -> b -> b
  
instance FstParamSet3 a (FreqWidthS a b) where
  set13 = freqWidthSSet1

instance FstParamSet3 a (Bass a b) where
  set13 = bassSet1

instance FstParamSet3 a (Treble a b) where
  set13 = trebleSet1

instance FstParamSet3 a (BendTrio a b) where
  set13 = bendTrioSet1

instance FstParamSet a (FrameRate a) where 
  set1 = frameRateSet1

instance FstParamSet a (OverSample a) where 
  set1 = overSampleSet1

instance FstParamSet3 a (Bend a b c) where
  set13 = bendSet1

instance FstParamSet3 a (Coeffs a) where
  set13 = coeffsSet1

instance FstParamSet3 (Coeffs a) (Biquad a) where
  set13 = biquadSet1

instance FstParamSet a (Chans a) where
  set1 = channelsSet1

instance FstParamSet3 a (ChorusTail a b) where
  set13 = chorusTailSet1 1  

instance FstParamSet3 a (Chorus a b) where
  set13 = chorusSet1 1

instance FstParamSet3 a (Contrast a) where
  set13 = contrastSet1
    
instance FstParamSet3 a (DCShift a b) where
  set13 = dcShiftSet1

instance FstParamSetL TSpecification Dlay where
  set1L = delaySet1

instance FstParamSet a (Filter a) where
  set1 = filterSet1

instance FstParamSet Float Precision where 
  set1 = precisionSet1

instance FstParamSet3 a (Dither a b c) where 
  set13 = ditherSet1

instance FstParamSet a (Downsample a) where
  set1 = downSampleSet1

instance FstParamSet3 a (EchoTail a) where
  set13 = echoTailSet1 1  

instance FstParamSet3 a (Echo a b) where
  set13 = echoSet1 1

instance FstParamSet3 a (Echos a b) where
  set13 = echosSet1 1  

instance FstParamSet3 a (Fade2 a b) where
  set13 = fadeSet1

instance FstParamSet3 String Fade where
  set13 = fadeSet2E 1  

instance FstParamSet a (Fir a b) where
  set1 = firSet1

instance FstParamSetL3 a (Flanger a b) where 
  set13L = flangerSet1
  
instance FstParamSet3 a (Gain1 a b c d) where
  set13 = gainSet1

instance FstParamSet a (Hilbert a) where
  set1 = HI

instance FstParamSet3 a (Ladspa1 a b c) where
  set13 = ladspaSet1

instance FstParamSet3 a (Loudness a) where
  set13 = loudnessSet1

instance FstParamSet a (FloatE a) where
  set1 = floatESet1

instance FstParamSet3O a (CompandTail a b) where 
  set1o = compandTailSet1

instance FstParamSet3 a (Pair a) where 
  set13 = pairSet1

instance FstParamSet3 a (AtDe a) where
  set13 = atDeSet1

instance FstParamSet a (Neg a) where
  set1 = NG

instance FstParamSet a (SoftKnee a) where
  set1 = SK

instance FstParamSet3 a (Compand a b c d) where
  set13 = compandSet1
    
instance FstParamSet3 a (FreqComp a b) where
  set13 = freqCompSet1

instance FstParamSet3 a (MCompand a b) where
  set13 = mCompandSet1

instance FstParamSetL a (MscS a) where
  set1L = mscSSet1

instance FstParamSet a (Noiseprof a) where
  set1 = noiseprofSet1

instance FstParamSet3 a (Noisered a b) where 
  set13 = noiseredSet1

instance FstParamSet3 a (Overdrive a) where 
  set13 = overdriveSet1

instance FstParamSet3 a (PadSpec a) where
  set13 = padSpecSet1

instance FstParamSet3O a (Pad a b) where
  set1o = padSet1

instance FstParamSet3 a (FreqWidth a b) where
  set13 = freqWidthSet1

instance FstParamSet3 a (Freq a) where
  set13 = freqSet1

instance FstParamSet3 a (AllPass a) where 
  set13 = allPassSet1

instance FstParamSet3 a (BandReject a) where
  set13 = bandRejectSet1
  
instance FstParamSet3 a (BandPass a b) where
  set13 = bandPassSet1

instance FstParamSet3 a (Band a b) where
  set13 = bandSet1

instance FstParamSet3 a (HighPass a b) where
  set13 = highPassSet1

instance FstParamSet3 a (LowPass a b) where
  set13 = lowPassSet1

instance FstParamSet3 a (Equalizer a b) where
  set13 = equalizerSet1

instance FstParamSet3 a (Phaser a b) where
  set13 = phaserSet1 1

instance FstParamSet3 a (Pitch a b c) where 
  set13 = pitchSet1

instance FstParamSet a (Ropt4 a) where 
  set1 = rOpt4Set1

instance FstParamSet a (Ropt5 a) where
  set1 = rOpt5Set1

instance FstParamSet3 a (RateL a b) where
  set13 = rateLSet1

instance FstParamSet3 a (RateH a b1 b2 b3 b4 b5 c) where
  set13 = rateHSet1

instance FstParamSet RateLow Rate where
  set1 = rate2Set1
    
instance FstParamSet3 Float (Vol3 Float) where
  set13 = vol3Set1

instance FstParamSet3 a (IChannel a b) where
  set13 = ichannelSet1

instance FstParamSetL3 a (OChannel a) where
  set13L = ochannelSet1

instance FstParamSet3 MixSpec ReMix where
  set13 = remixSet1

instance FstParamSet a (Repeat a) where 
  set1 = Rpt

instance FstParamSet3 a (Reverb a b c d) where 
  set13 = reverbSet1

instance FstParamSet3 Float ReverbE where
  set13 = reverbSet3E 1  

instance FstParamSet3 a (Segment a) where
  set13 = segmentSet1

instance FstParamSet3 a (Threshold a) where
  set13 = thresholdSet1

instance FstParamSet3 a (Duration a b) where
  set13 = durationSet1d

instance FstParamSet3 a (AboveTSpec1 a b c) where
  set13 = aboveTSpecSet1a

instance FstParamSet3 a (BelowTSpec1 a b c) where 
  set13 = belowTSpecSet1b

instance FstParamSet3 a (Silence a b c) where
  set13 = silenceSet1
  
instance FstParamSet a (PhaseR a) where
  set1 = phaseRSet1

instance FstParamSet a (FreqL a) where
  set1 = freqLSet1

instance FstParamSet a (FreqH a) where
  set1 = freqHSet1

instance FstParamSet3 a (Sinc a b c d) where 
  set13 = sincSet1

instance FstParamSet3 Float Freq1 where 
  set13 = frequencySet1

instance FstParamSet3 a (Width a) where
  set13 = widthSet1

instance FstParamSet3 a (SFloat1 a) where
  set13 = sFloat1Set1

instance FstParamSet3 a (SString1 a) where
  set13 = sString1Set1

instance FstParamSet a (Advanced1 a) where
  set1 = advanced1Set1

instance FstParamSet Float FirstDTSpec where
  set1 = seconds2FstDTSpec2
    
instance FstParamSet Int FirstDTSpec where
  set1 = samples2FstDTSpec2

instance FstParamSetL3 a (Spectrogram3 a b c d e) where
  set13L = spectrogramSet31

instance FstParamSet3 a (Speed a b) where
  set13 = speedSet1

instance FstParamSet3 a (Splice2 a b) where
  set13 = splice2Set1

instance FstParamSet a (StatP a) where
  set1 = statPSet1

instance FstParamSetL a (Stat1 a) where 
  set1L = stat1Set1

instance FstParamSet3 a (StatsP a) where 
  set13 = statsPSet1

instance FstParamSet a (Window1 a) where
  set1 = window1Set1

instance FstParamSetL3 a (Stats2 a b) where
  set13L = stats2Set1

instance FstParamSet3 a (StretchP a) where
  set13 = stretchSet1

instance FstParamSet3 a (Stretch2 a b) where
  set13 = stretch2Set1

instance FstParamSet3 a (Tempo a b c d) where 
  set13 = tempoSet1

instance FstParamSet3 a (TimeSpec a b) where
  set13 = timeSpecSet1

instance FstParamSet3 a (Tremolo a) where
  set13 = tremoloSet1

instance FstParamSet a (Trim a) where 
  set1 = trimSet1

instance FstParamSet a (Upsample a) where 
  set1 = upSampleSet1

instance FstParamSet3 a (VadP a) where
  set13 = vadPSet1

instance FstParamSetL3 a (Vad1 a) where
  set13L = vad1Set1

instance FstParamSet3 a (Vol2 a b) where
  set13 = volSet1

instance FstParamSet3 a (One2 a) where
  set13 = one2Set1

instance FstParamSet3 a (One3 a) where
  set13 = one3Set1

------------------------------------------------------------------------------------------
