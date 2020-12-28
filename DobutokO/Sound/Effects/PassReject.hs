-- |
-- Module      :  DobutokO.Sound.Effects.PassReject
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects with the needed specifications. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.PassReject where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.Effects.Specs
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data FreqWidth a b = Fr1 a | FrW2 a b deriving Eq

instance Show (FreqWidth Freq1 Width1) where
  show (Fr1 x) = show x
  show (FrW2 x y) = mconcat [show x," ",show y]

type FreqW2 = FreqWidth Freq1 Width1   

freqWidthC :: FreqWidth a b -> String
freqWidthC (Fr1 _) = "Fr1"
freqWidthC (FrW2 _ _) = "FrW2"

freqWidth1 :: FreqWidth a b -> a
freqWidth1 (Fr1 x) = x
freqWidth1 (FrW2 x _) = x

freqWidth2 :: FreqWidth a b -> Maybe b
freqWidth2 (Fr1 _) = Nothing
freqWidth2 (FrW2 _ y) = Just y

freqWidthSet1 :: a -> FreqWidth a b -> FreqWidth a b
freqWidthSet1 x (Fr1 _) = Fr1 x
freqWidthSet1 x (FrW2 _ y) = FrW2 x y

freqWidthSet2 :: b -> FreqWidth a b -> FreqWidth a b
freqWidthSet2 y (Fr1 x) = FrW2 x y
freqWidthSet2 y (FrW2 x _) = FrW2 x y

data Freq a = Fr a deriving Eq

instance Show (Freq Freq1) where
  show (Fr x) = show x

type Freq11 = Freq Freq1

freqC :: Freq a -> String
freqC (Fr _) = "Fr"

freq1 :: Freq a -> a
freq1 (Fr x) = x

freqSet1 :: a -> Freq a -> Freq a
freqSet1 x (Fr _) = Fr x

data AllPass a = AP a deriving Eq

instance Show (AllPass FreqW2) where
  show (AP x) = mconcat ["allpass ",show x]

type Allpass = AllPass FreqW2

allPass1 :: AllPass a -> a
allPass1 (AP x) = x

allPassSet1 :: a -> AllPass a -> AllPass a
allPassSet1 x (AP _) = AP x

showApQ :: Allpass -> [String]
showApQ = words . show

data BandReject a = BR a deriving Eq

instance Show (BandReject FreqW2) where
  show (BR x) = mconcat ["bandreject ",show x]

type Bandreject = BandReject FreqW2

bandReject1 :: BandReject a -> a
bandReject1 (BR x) = x

bandRejectSet1 :: a -> BandReject a -> BandReject a
bandRejectSet1 x (BR _) = BR x

showBrQ :: Bandreject -> [String]
showBrQ = words . show

data BandpassPar = C0 | C deriving Eq

instance Show BandpassPar where
  show C0 = ""
  show C = "-c "

data BandPass a b = BP a b deriving Eq

instance Show (BandPass BandpassPar FreqW2) where
  show (BP x y) = mconcat ["bandpass ",show x,show y]

type Bandpass = BandPass BandpassPar FreqW2

bandPass1 :: BandPass a b -> a
bandPass1 (BP x _) = x

bandPass2 :: BandPass a b -> b
bandPass2 (BP _ y) = y

bandPassSet1 :: a -> BandPass a b -> BandPass a b
bandPassSet1 x (BP _ y) = BP x y

bandPassSet2 :: b -> BandPass a b -> BandPass a b
bandPassSet2 y (BP x _) = BP x y

showBpQ :: Bandpass -> [String]
showBpQ = words . show  

data BandPar = N0 | N deriving Eq

instance Show BandPar where
  show N0 = ""
  show N = "-n "

data Band a b = B a b deriving Eq

instance Show (Band BandPar FreqW2) where
  show (B x y) = mconcat ["band ",show x,show y]

type Band2 = Band BandPar FreqW2

band1 :: Band a b -> a
band1 (B x _) = x

band2 :: Band a b -> b
band2 (B _ y) = y

bandSet1 :: a -> Band a b -> Band a b
bandSet1 x (B _ y) = B x y

bandSet2 :: b -> Band a b -> Band a b
bandSet2 y (B x _) = B x y

showB2Q :: Band2 -> [String]
showB2Q = words . show  

data HighLowPar = HL1 | HL2 deriving Eq

instance Show HighLowPar where
 show HL1 = "-1 "
 show HL2 = "-2 "

data HighPass a b = HP a b deriving Eq

instance Show (HighPass HighLowPar FreqW2) where
  show (HP x y) = mconcat ["highpass ",show x,show y]

type HighPass2 = HighPass HighLowPar FreqW2

highPass1 :: HighPass a b -> a
highPass1 (HP x _) = x

highPass2 :: HighPass a b -> b
highPass2 (HP _ y) = y

highPassSet1 :: a -> HighPass a b -> HighPass a b
highPassSet1 x (HP _ y) = HP x y

highPassSet2 :: b -> HighPass a b -> HighPass a b
highPassSet2 y (HP x _) = HP x y

showHpQ :: HighPass2 -> [String]
showHpQ = words . show  
 
data LowPass a b = LP a b deriving Eq

instance Show (LowPass HighLowPar FreqW2) where
  show (LP x y) = mconcat ["lowpass ",show x,show y]

type LowPass2 = LowPass HighLowPar FreqW2

lowPass1 :: LowPass a b -> a
lowPass1 (LP x _) = x

lowPass2 :: LowPass a b -> b
lowPass2 (LP _ y) = y

lowPassSet1 :: a -> LowPass a b -> LowPass a b
lowPassSet1 x (LP _ y) = LP x y

lowPassSet2 :: b -> LowPass a b -> LowPass a b
lowPassSet2 y (LP x _) = LP x y

showLpQ :: LowPass2 -> [String]
showLpQ = words . show  
 
data Equalizer a b = Eqlz a b deriving Eq

instance Show (Equalizer FreqW2 Float) where
  show (Eqlz x y) = mconcat ["equalizer ",show x," ",showFFloat Nothing y " "]

type Equaliz = Equalizer FreqW2 Float

equalizer1 :: Equalizer a b -> a
equalizer1 (Eqlz x _) = x

equalizer2 :: Equalizer a b -> b
equalizer2 (Eqlz _ y) = y

equalizerSet1 :: a -> Equalizer a b -> Equalizer a b
equalizerSet1 x (Eqlz _ y) = Eqlz x y

equalizerSet2 :: b -> Equalizer a b -> Equalizer a b
equalizerSet2 y (Eqlz x _) = Eqlz x y

showEqlQ :: Equaliz -> [String]
showEqlQ = words . show  
  
