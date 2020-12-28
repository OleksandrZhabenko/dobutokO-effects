-- |
-- Module      :  DobutokO.Sound.Effects.Classes.SndParam
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

module DobutokO.Sound.Effects.Classes.SndParam where

import DobutokO.Sound.Effects.Splice
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
import DobutokO.Sound.Effects.Segment
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Speed
import DobutokO.Sound.Effects.Stats
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.Effects.Tremolo
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One

class SndParam a b where
  get2 :: a -> b

class SndParamL a b where
  get2L :: a -> [b]

class SndParamM a b where
  get2m :: a -> Maybe b  
  
instance SndParamM (FreqWidthS a b) b where
  get2m = freqWidthS2

instance SndParam (Bass a b) b where
  get2 = bass2

instance SndParam (Treble a b) b where
  get2 = treble2

instance SndParam (BendTrio a b) b where
  get2 = bendTrio2

instance SndParamM (Bend a b c) b where
  get2m = bend2

instance SndParamM (Coeffs a) a where
  get2m = coeffs1 2 

instance SndParam (Biquad a) (Coeffs a) where
  get2 = biquad2

instance SndParam (ChorusTail a b) a where
  get2 = chorusTail1 2

instance SndParam (Chorus a b) a where
  get2 = chorus1 2  

instance SndParamM (DCShift a b) b where
  get2m = dcShift2

instance SndParamM (Dither a b c) b where 
  get2m = dither2

instance SndParam (EchoTail a) a where
  get2 = echoTail1 2 

instance SndParam (Echo a b) a where
  get2 = echo1 2

instance SndParam (Echos a b) a where
  get2 = echos1 2

instance SndParamL (Fade2 a b) b where
  get2L = fade2

instance SndParam Fade String where
  get2 = fade2E 2

instance SndParamM (Fir a b) [b] where
  get2m = fir2

instance SndParam (Flanger a b) b where 
  get2 = flanger2

instance SndParam (Gain1 a b c d) b where
  get2 = gain2

instance SndParam (Ladspa1 a b c) (One2 b) where
  get2 = ladspa2

instance SndParamM (Loudness a) a where
  get2m = loudness2

instance SndParamM (CompandTail a b) b where 
  get2m = compandTail2

instance SndParam (Pair a) a where 
  get2 = pair2

instance SndParamL (AtDe a) a where
  get2L = atDe2

instance SndParam (Compand a b c d) b where
  get2 = compand2

instance SndParam (FreqComp a b) b where
  get2 = freqComp2

instance SndParamM (MCompand a b) [b] where
  get2m = mCompand2

instance SndParamM (Noisered a b) b where 
  get2m = noisered2

instance SndParamM (Overdrive a) a where 
  get2m = overdrive2

instance SndParam (PadSpec a) a where
  get2 = padSpec2

instance SndParamL (Pad a b) b where
  get2L = pad2

instance SndParamM (FreqWidth a b) b where
  get2m = freqWidth2

instance SndParam (BandPass a b) b where
  get2 = bandPass2

instance SndParam (Band a b) b where
  get2 = band2

instance SndParam (HighPass a b) b where
  get2 = highPass2

instance SndParam (LowPass a b) b where
  get2 = lowPass2

instance SndParam (Equalizer a b) b where
  get2 = equalizer2

instance SndParam (Phaser a b) a where
  get2 = phaser1 2

instance SndParam (Pitch a b c) b where 
  get2 = pitch2

instance SndParam (RateL a b) b where
  get2 = rateL2

instance SndParam (RateH a b1 b2 b3 b4 b5 c) b1 where
  get2 = rateH21

instance SndParamM (Rate2 a b) b where
  get2m = rate22
    
instance SndParamL (Remix a b) b where
  get2L = remix2

instance SndParam (Reverb a b c d) b where 
  get2 = reverb2

instance SndParam ReverbE Float where
  get2 = reverb3E 2  

instance SndParamM (Segment a) a where
  get2m = segment2

instance SndParamM (Duration a b) b where
  get2m = duration2

instance SndParamM (AboveTSpec1 a b c) b where
  get2m = aboveTSpec2

instance SndParamM (BelowTSpec1 a b c) b where 
  get2m = belowTSpec2

instance SndParam (Silence a b c) b where
  get2 = silence2
  
instance SndParam (Sinc a b c d) b where 
  get2 = sinc2

instance SndParamM FirstDTSpec Int where
  get2m = minutesD
    
instance SndParamL (Spectrogram3 a b c d e) b where
  get2L = spectrogram32

instance SndParam (Speed a b) b where
  get2 = speed2

instance SndParamL (Splice2 a b) (One3 b) where
  get2L = splice22

instance SndParamL (Stats2 a b) b where
  get2L = stats22

instance SndParam (StretchP a) a where
  get2 = stretch2

instance SndParamM (Stretch2 a b) b where
  get2m = stretch22

instance SndParam (Tempo a b c d) b where 
  get2 = tempo2

instance SndParamM FirstTSpec Int where
  get2m = minutes
  
instance SndParamM NextTSpec Int where
  get2m = minutes2

instance SndParamM (TimeSpec a b) [b] where
  get2m = timeSpec2

instance SndParamM (Tremolo a) a where
  get2m = tremolo2

instance SndParamM (Vol2 a b) b where
  get2m = vol2


