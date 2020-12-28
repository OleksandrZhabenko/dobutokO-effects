-- |
-- Module      :  DobutokO.Sound.Effects.Classes.ThdParamSet
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

module DobutokO.Sound.Effects.Classes.ThdParamSet where

-- inspired with: https://wiki.haskell.org/Scoped_type_variables
import GHC.Base (asTypeOf)
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
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Stretch
import DobutokO.Sound.Effects.Tempo
import DobutokO.Sound.Effects.Vol
import DobutokO.Sound.One

class ThdParamSet3 a b where
  set33 :: a -> b -> b
    
class ThdParamSetL3 a b where
  set33L :: [a] -> b -> b

class ThdParamSet3O a b where
  set33o :: (One2 a) -> b -> b

instance ThdParamSet3 a (BendTrio a b) where
  set33 = bendTrioSet3

instance ThdParamSet3 c (Bend a b c) where
  set33 = bendSet3

instance ThdParamSet3 a (Coeffs a)  where
  set33 = coeffsSet3 

instance ThdParamSet3 a (ChorusTail a b)  where
  set33 = chorusTailSet1 3

instance ThdParamSetL3 b (Chorus a b)  where
  set33L = chorusSet2  

instance ThdParamSet3 c (Dither a b c) where 
  set33 = ditherSet3

instance ThdParamSetL3 b (Echo a b)  where
  set33L = echoSet2

instance ThdParamSetL3 b (Echos a b)  where
  set33L = echosSet2

instance ThdParamSet3 String Fade where
  set33 = fadeSet2E 3

instance ThdParamSet3 c (Gain1 a b c d) where
  set33 = gainSet3

instance (Show c) => ThdParamSet3 c (Ladspa1 a b c) where
  set33 = ladspaSet3

instance ThdParamSet3 c (Compand a b c d) where
  set33 = compandSet3

instance ThdParamSet3 a (Phaser a b)  where
  set33 = phaserSet1 3

instance ThdParamSet3 c (Pitch a b c) where 
  set33 = pitchSet3

instance ThdParamSet3 b2 (RateH a b1 b2 b3 b4 b5 c) where
  set33 = rateHSet22

instance ThdParamSetL3 c (Reverb a b c d) where 
  set33L = reverbSet3

instance ThdParamSet3 Float ReverbE where
  set33 = reverbSet3E 3 

instance ThdParamSet3 c (AboveTSpec1 a b c) where
  set33 = aboveTSpecSet3a

instance ThdParamSet3 c (BelowTSpec1 a b c) where 
  set33 = belowTSpecSet3b

instance ThdParamSet3 c (Silence a b c) where
  set33 = silenceSet3
  
instance ThdParamSet3O c (Sinc a b c d) where 
  set33o = sincSet3
    
instance ThdParamSetL3 c (Spectrogram3 a b c d e) where
  set33L = spectrogramSet33

instance ThdParamSet3 a (StretchP a)  where
  set33 = stretchSet3

instance ThdParamSet3 c (Tempo a b c d) where 
  set33 = tempoSet3

instance ThdParamSet3 Float Vol where
  set33 = volSet3
