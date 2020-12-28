-- |
-- Module      :  DobutokO.Sound.Effects.Classes.FourthParam
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

module DobutokO.Sound.Effects.Classes.FourthParam where

import DobutokO.Sound.Effects.Chorus
import DobutokO.Sound.Effects.Gain
import DobutokO.Sound.Effects.MCompand
import DobutokO.Sound.Effects.Phaser
import DobutokO.Sound.Effects.Rate
import DobutokO.Sound.Effects.Reverb
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Spectrogram
import DobutokO.Sound.Effects.Tempo

class FourthParam a b where
  get4 :: a -> b

class FourthParamL a b where
  get4L :: a -> [b]

class FourthParamM a b where
  get4m :: a -> Maybe b  
  
instance FourthParam (ChorusTail a b) a where
  get4 = chorusTail1 4

instance FourthParam (Gain1 a b c d) d where
  get4 = gain4

instance FourthParamM (Compand a b c d) d where
  get4m = compand4

instance FourthParam (Phaser a b) a where
  get4 = phaser1 4

instance FourthParam (RateH a b1 b2 b3 b4 b5 c) b3 where
  get4 = rateH23

instance FourthParam (Reverb a b c d) d where 
  get4 = reverb4

instance FourthParam ReverbE Float where
  get4 = reverb3E 4
  
instance FourthParam (Sinc a b c d) d where 
  get4 = sinc4

instance FourthParamL (Spectrogram3 a b c d e) d where
  get4L = spectrogram34

instance FourthParamM (Tempo a b c d) d where 
  get4m = tempo4

------------------------------------------------------------------------------------------

class FifthParam a b where
  get5 :: a -> b

class FifthParamL a b where
  get5L :: a -> [b]

instance FifthParam (Phaser a b) a where
  get5 = phaser1 5

instance FifthParam (RateH a b1 b2 b3 b4 b5 c) b4 where
  get5 = rateH24

instance FifthParam ReverbE Float where
  get5 = reverb3E 5

instance FifthParamL (Spectrogram3 a b c d e) e where
  get5L = spectrogram35

------------------------------------------------------------------------------------------

class SixthParam a b where
  get6 :: a -> b

instance SixthParam (Phaser a b) b where
  get6 = phaser2

instance SixthParam (RateH a b1 b2 b3 b4 b5 c) b5 where
  get6 = rateH25

instance SixthParam ReverbE Float where
  get6 = reverb3E 6

------------------------------------------------------------------------------------------

class SeventhParam a b where
  get7 :: a -> b

instance SeventhParam (RateH a b1 b2 b3 b4 b5 c) c where
  get7 = rateH3

-- ========================================================================================

