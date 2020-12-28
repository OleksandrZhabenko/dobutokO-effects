-- |
-- Module      :  DobutokO.Sound.Effects.Classes.FstParam
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

module DobutokO.Sound.Effects.Classes.FstParam where

import DobutokO.Sound.Effects.Splice
import DobutokO.Sound.Effects.Vad
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
import DobutokO.Sound.Effects.Stat
import DobutokO.Sound.Effects.Stats
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.Effects.Tremolo
import DobutokO.Sound.Effects.Trim
import DobutokO.Sound.Effects.Upsample
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One


class FstParam a b where
  get1 :: a -> b

class FstParamL a b where
  get1L :: a -> [b]

class FstParamM a b where
  get1m :: a -> Maybe b  
  
instance FstParam (FreqWidthS a b) a where
  get1 = freqWidthS1

instance FstParam (Bass a b) a where
  get1 = bass1

instance FstParam (Treble a b) a where
  get1 = treble1

instance FstParam (BendTrio a b) a where
  get1 = bendTrio1

instance FstParam (FrameRate a) a where 
  get1 = frameRate1

instance FstParam (OverSample a) a where 
  get1 = overSample1

instance FstParamM (Bend a b c) a where
  get1m = bend1

instance FstParamM (Coeffs a) a where
  get1m = coeffs1 1

instance FstParam (Biquad a) (Coeffs a) where
  get1 = biquad1

instance FstParam (Chans a) a where
  get1 = channels1

instance FstParam (ChorusTail a b) a where
  get1 = chorusTail1 1  

instance FstParam (Chorus a b) a where
  get1 = chorus1 1

instance FstParamM (Contrast a) a where
  get1m = contrast1
    
instance FstParam Cntrst Float where
  get1 = contrastE1

instance FstParam (DCShift a b) a where
  get1 = dcShift1

instance FstParamL Dlay TSpecification where
  get1L = delay1

instance FstParamM (Filter a) a where
  get1m = filter1

instance FstParamM FilterN NoiseType where
  get1m = filterN1

instance FstParamM (PrecisionD a) a where 
  get1m = precisionD1

instance FstParamM (Dither a b c) a where 
  get1m = dither1

instance FstParamM (Downsample a) a where
  get1m = downSample1

instance FstParam DSample Int where
  get1 = downSampleE1

instance FstParam (EchoTail a) a where
  get1 = echoTail1 1  

instance FstParam (Echo a b) a where
  get1 = echo1 1

instance FstParam (Echos a b) a where
  get1 = echos1 1  

instance FstParam (Fade2 a b) a where
  get1 = fade1

instance FstParam Fade String where
  get1 = fade2E 1  

instance FstParamM (Fir a b) a where
  get1m = fir1

instance FstParamL (Flanger a b) a where 
  get1L = flanger1

instance FstParamL Flanger2 Float where
  get1L = flanger1E
  
instance FstParam (Gain1 a b c d) a where
  get1 = gain1

instance FstParamM (Hilbert a) a where
  get1m = hilbert1

instance FstParam (Ladspa1 a b c) a where
  get1 = ladspa1

instance FstParamM (Loudness a) a where
  get1m = loudness1

instance FstParamM (FloatE a) a where
  get1m = floatE1

instance FstParamM (CompandTail a b) (One2 a) where 
  get1m = compandTail1

instance FstParam (Pair a) a where 
  get1 = pair1

instance FstParam (AtDe a) a where
  get1 = atDe1

instance FstParam (Neg a) a where
  get1 = neg1

instance FstParamM (SoftKnee a) a where
  get1m = softKnee1

instance FstParam (Compand a b c d) a where
  get1 = compand1
    
instance FstParam KFQ Int where
  get1 = kFreq1

instance FstParam (FreqComp a b) a where
  get1 = freqComp1

instance FstParam (MCompand a b) a where
  get1 = mCompand1

instance FstParamL (MscS a) a where
  get1L = mscS1

instance FstParamM (Noiseprof a) a where
  get1m = noiseprof1

instance FstParamM (Noisered a b) a where 
  get1m = noisered1

instance FstParamM (Overdrive a) a where 
  get1m = overdrive1

instance FstParam (PadSpec a) a where
  get1 = padSpec1

instance FstParamL (Pad a b) a where
  get1L = pad1

instance FstParam (FreqWidth a b) a where
  get1 = freqWidth1

instance FstParam (Freq a) a where
  get1 = freq1

instance FstParam (AllPass a) a where 
  get1 = allPass1

instance FstParam (BandReject a) a where
  get1 = bandReject1
  
instance FstParam (BandPass a b) a where
  get1 = bandPass1

instance FstParam (Band a b) a where
  get1 = band1

instance FstParam (HighPass a b) a where
  get1 = highPass1

instance FstParam (LowPass a b) a where
  get1 = lowPass1

instance FstParam (Equalizer a b) a where
  get1 = equalizer1

instance FstParam (Phaser a b) a where
  get1 = phaser1 1

instance FstParam (Pitch a b c) a where 
  get1 = pitch1

instance FstParamM (Ropt4 a) a where 
  get1m = rOpt41

instance FstParamM (Ropt5 a) a where
  get1m = rOpt51

instance FstParam (RateL a b) a where
  get1 = rateL1

instance FstParam (RateH a b1 b2 b3 b4 b5 c) a where
  get1 = rateH1

instance FstParamM (Rate2 a b) a where
  get1m = rate21
    
instance FstParam (Vol3 Float) Float where
  get1 = vol31

instance FstParam (IChannel a b) a where
  get1 = ichannel1

instance FstParam (IChannel a Float) Float where
  get1 = ichannel21

instance FstParamL (OChannel a) a where
  get1L = ochannel1

instance FstParamM (Remix a b) a where
  get1m = remix1

instance FstParam (Repeat a) a where 
  get1 = repeat1

instance FstParam (Reverb a b c d) a where 
  get1 = reverb1

instance FstParam ReverbE Float where
  get1 = reverb3E 1  

instance FstParamM (Segment a) a where
  get1m = segment1

instance FstParam (Threshold a) a where
  get1 = threshold1

instance FstParamM (Duration a b) a where
  get1m = duration1

instance FstParamM (AboveTSpec1 a b c) a where
  get1m = aboveTSpec1

instance FstParamM (BelowTSpec1 a b c) a where 
  get1m = belowTSpec1

instance FstParam (Silence a b c) a where
  get1 = silence1
  
instance FstParamM (PhaseR a) a where
  get1m = phaseR1

instance FstParamM (SincAB a) a where
  get1m = sincAB1

instance FstParamM (SincTN a) a where
  get1m = sincTN1

instance FstParam (FreqL a) a where
  get1 = freqL1

instance FstParam (FreqH a) a where
  get1 = freqH1

instance FstParam (Sinc a b c d) a where 
  get1 = sinc1

instance FstParam Freq1 Float where 
  get1 = frequency1

instance FstParam (Width a) a where
  get1 = width1

instance FstParam (SFloat1 a) a where
  get1 = sFloat11

instance FstParam (SString1 a) a where
  get1 = sString11

instance FstParam (Advanced1 a) a where
  get1 = advanced11

instance FstParamM FirstDTSpec Float where
  get1m = secondsD
    
instance FstParamM FirstDTSpec Int where
  get1m = samplesD

instance FstParamL (Spectrogram3 a b c d e) a where
  get1L = spectrogram31

instance FstParam (Speed a b) a where
  get1 = speed1

instance FstParam (Splice2 a b) a where
  get1 = splice21

instance FstParamM (StatP a) a where
  get1m = statP1

instance FstParamL (Stat1 a) a where 
  get1L = stat11

instance FstParamM (StatsP a) a where 
  get1m = statsP1

instance FstParamM (Window1 a) a where
  get1m = window11

instance FstParamL (Stats2 a b) a where
  get1L = stats21

instance FstParam (StretchP a) a where
  get1 = stretch1

instance FstParam (Stretch2 a b) a where
  get1 = stretch21

instance FstParam (Tempo a b c d) a where 
  get1 = tempo1

instance FstParamM FirstTSpec Float where
  get1m = seconds
  
instance FstParamM FirstTSpec Int where
  get1m = samples

instance FstParamM NextTSpec Float where
  get1m = seconds2
  
instance FstParamM NextTSpec Int where
  get1m = samples2

instance FstParam (TimeSpec a b) a where
  get1 = timeSpec1

instance FstParam (Tremolo a) a where
  get1 = tremolo1

instance FstParam (Trim a) a where 
  get1 = trim1

instance FstParamM (Upsample a) a where 
  get1m = upSample1

instance FstParam (VadP a) a where
  get1 = vadP1

instance FstParamL (Vad1 a) a where
  get1L = vad11

instance FstParam (Vol2 a b) a where
  get1 = vol1

instance FstParamL (One2 a) a where
  get1L = one21

instance FstParamL (One3 a) a where
  get1L = one31

------------------------------------------------------------------------------------------
