-- |
-- Module      :  DobutokO.Sound.Effects.Classes.FourthParamSet
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

module DobutokO.Sound.Effects.Classes.FourthParamSet where

import DobutokO.Sound.Effects.Chorus
import DobutokO.Sound.Effects.Gain
import DobutokO.Sound.Effects.MCompand
import DobutokO.Sound.Effects.Phaser
import DobutokO.Sound.Effects.Rate
import DobutokO.Sound.Effects.Reverb
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Tempo

class FourthParamSet3 a b where
  set43 :: a -> b -> b

class FourthParamSetL3 a b where
  set43L :: [a] -> b -> b
  
instance FourthParamSet3 a (ChorusTail a b) where
  set43 = chorusTailSet1 4

instance FourthParamSet3 d (Gain1 a b c d) where
  set43 = gainSet4

instance FourthParamSet3 d (Compand a b c d) where
  set43 = compandSet4

instance FourthParamSet3 a (Phaser a b) where
  set43 = phaserSet1 4

instance FourthParamSet3 b3 (RateH a b1 b2 b3 b4 b5 c) where
  set43 = rateHSet23

instance FourthParamSet3 d (Reverb a b c d) where 
  set43 = reverbSet4

instance FourthParamSet3 Float ReverbE where
  set43 = reverbSet3E 4
  
instance FourthParamSet3 d (Sinc a b c d) where 
  set43 = sincSet4

instance FourthParamSetL3 d (Spectrogram3 a b c d e) where
  set43L = spectrogramSet34

instance FourthParamSet3 d (Tempo a b c d) where 
  set43 = tempoSet4

------------------------------------------------------------------------------------------

class FifthParamSet3 a b where
  set53 :: a -> b -> b

class FifthParamSetL3 a b where
  set53L :: [a] -> b -> b

instance FifthParamSet3 a (Phaser a b) where
  set53 = phaserSet1 5

instance FifthParamSet3 b4 (RateH a b1 b2 b3 b4 b5 c) where
  set53 = rateHSet24

instance FifthParamSet3 Float ReverbE where
  set53 = reverbSet3E 5

instance FifthParamSetL3 e (Spectrogram3 a b c d e) where
  set53L = spectrogramSet35

------------------------------------------------------------------------------------------

class SixthParamSet3 a b where
  set63 :: a -> b -> b

instance SixthParamSet3 b (Phaser a b) where
  set63 = phaserSet2

instance SixthParamSet3 b5 (RateH a b1 b2 b3 b4 b5 c) where
  set63 = rateHSet25

instance SixthParamSet3 Float ReverbE where
  set63 = reverbSet3E 6

------------------------------------------------------------------------------------------

class SeventhParamSet3 a b where
  set73 :: a -> b -> b

instance SeventhParamSet3 c (RateH a b1 b2 b3 b4 b5 c) where
  set73 = rateHSet3

-- ========================================================================================

