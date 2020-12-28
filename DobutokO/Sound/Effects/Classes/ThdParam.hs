-- |
-- Module      :  DobutokO.Sound.Effects.Classes.ThdParam
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

module DobutokO.Sound.Effects.Classes.ThdParam where

import DobutokO.Sound.Effects.Bend
import DobutokO.Sound.Effects.Biquad
import DobutokO.Sound.Effects.Chorus
import DobutokO.Sound.Effects.Dither
import DobutokO.Sound.Effects.Echo
import DobutokO.Sound.Effects.Fade
import DobutokO.Sound.Effects.Gain
import DobutokO.Sound.Effects.LADSPA
import DobutokO.Sound.Effects.MCompand
import DobutokO.Sound.Effects.Phaser
import DobutokO.Sound.Effects.Pitch
import DobutokO.Sound.Effects.Rate
import DobutokO.Sound.Effects.Reverb
import DobutokO.Sound.Effects.Segment
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One

class ThdParam a b where
  get3 :: a -> b

class ThdParamL a b where
  get3L :: a -> [b]

class ThdParamM a b where
  get3m :: a -> Maybe b  
  
instance ThdParam (BendTrio a b) a where
  get3 = bendTrio3

instance ThdParam (Bend a b c) c where
  get3 = bend3

instance ThdParamM (Coeffs a) a where
  get3m = coeffs1 3

instance ThdParam (ChorusTail a b) a where
  get3 = chorusTail1 3

instance ThdParamL (Chorus a b) b where
  get3L = chorus2

instance ThdParamM (Dither a b c) c where 
  get3m = dither3

instance ThdParamL (Echo a b) b where
  get3L = echo2

instance ThdParamL (Echos a b) b where
  get3L = echos2  

instance ThdParam Fade String where
  get3 = fade2E 3

instance ThdParam (Gain1 a b c d) c where
  get3 = gain3

instance ThdParamM (Ladspa1 a b c) c where
  get3m = ladspa3

instance ThdParam (Compand a b c d) c where
  get3 = compand3

instance ThdParam (Phaser a b) a where
  get3 = phaser1 3

instance ThdParamM (Pitch a b c) c where 
  get3m = pitch3

instance ThdParam (RateH a b1 b2 b3 b4 b5 c) b2 where
  get3 = rateH22

instance ThdParamL (Reverb a b c d) c where 
  get3L = reverb3

instance ThdParam ReverbE Float where
  get3 = reverb3E 3

instance ThdParamM (Segment a) a where
  get3m = segment3

instance ThdParamM (AboveTSpec1 a b c) c where
  get3m = aboveTSpec3

instance ThdParamM (BelowTSpec1 a b c) c where 
  get3m = belowTSpec3

instance ThdParamM (Silence a b c) c where
  get3m = silence3
  
instance ThdParam (Sinc a b c d) (One2 c) where 
  get3 = sinc3

instance ThdParamM FirstDTSpec Int where
  get3m = hoursD
    
instance ThdParamL (Spectrogram3 a b c d e) c where
  get3L = spectrogram33

instance ThdParam (StretchP a) a where
  get3 = stretch3

instance ThdParam (Tempo a b c d) c where 
  get3 = tempo3

instance ThdParamM FirstTSpec Int where
  get3m = hours
  
instance ThdParamM NextTSpec Int where
  get3m = hours2

instance ThdParamM (Vol2 a b) a where
  get3m = vol3

