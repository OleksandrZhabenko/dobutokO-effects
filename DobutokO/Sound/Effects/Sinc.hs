-- |
-- Module      :  DobutokO.Sound.Effects.Sinc
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"sinc\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}

module DobutokO.Sound.Effects.Sinc where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import DobutokO.Sound.Effects.Specs (Freq1)
import DobutokO.Sound.One

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data PhaseR a = P a | M | I | L deriving Eq

instance Show (PhaseR Float) where
  show (P x) = mconcat ["-p ", showFFloat Nothing (toRange 100.0 (abs x)) " "]
  show M = "-M "
  show I = "-I "
  show _ = "-L "

type Phase1 = PhaseR Float

phaseRC :: PhaseR a -> String  
phaseRC M = "M"
phaseRC I = "I"
phaseRC L = "L"
phaseRC _ = "P"

phaseR1 :: PhaseR a -> Maybe a
phaseR1 (P x) = Just x
phaseR1 _ = Nothing

phaseRSet1 :: a -> PhaseR a
phaseRSet1 = P

data SincAB a = N1 | A a | B a deriving Eq

instance Show (SincAB Float) where
  show (A x) = mconcat ["-a ", if compare (toRange 180.0 . abs $ x) 40.0 == LT then "40 " else showFFloat Nothing (toRange 180.0 . abs $ x) " "]
  show (B x) = mconcat ["-b ", showFFloat Nothing (toRange 256.0 . abs $ x) " "]
  show _ = ""

type Sinc1 = SincAB Float

sincABC :: SincAB a -> String
sincABC (A _) = "A"
sincABC (B _) = "B"
sincABC _ = "N1"

sincAB1 :: SincAB a -> Maybe a
sincAB1 (A x) = Just x
sincAB1 (B x) = Just x
sincAB1 _ = Nothing

sincABSet1 :: Bool -> a -> SincAB a
sincABSet1 True x = A x
sincABSet1 False x = B x

data SincTN a = N2 | T a | N a deriving Eq

instance Show (SincTN Float) where
  show (T x) = mconcat ["-t ", if compare (abs x) 1.0 == LT then "1 " else showFFloat Nothing (abs x) " "]
  show (N x) = mconcat ["-n ", if compare (toRange 32767.0 . abs $ x) 11.0 == LT then "11 " else showFFloat Nothing (toRange 32767.0 . abs $ x) " "]
  show _ = ""

type Sinc2 = SincTN Float

sincTNC :: SincTN a -> String
sincTNC (T _) = "T"
sincTNC (N _) = "N"
sincTNC _ = "N2"

sincTN1 :: SincTN a -> Maybe a
sincTN1 (T x) = Just x
sincTN1 (N x) = Just x
sincTN1 _ = Nothing

sincTNSet1 :: Bool -> a -> SincTN a
sincTNSet1 True x = T x
sincTNSet1 False x = N x

data FreqL a = LF a deriving Eq

instance Show (FreqL Freq1) where
  show (LF x) = mconcat ["-", show x]

type FreqFL = FreqL Freq1

freqL1 :: FreqL a -> a
freqL1 (LF x) = x

freqLSet1 :: a -> FreqL a
freqLSet1 = LF

data FreqH a =  HF a deriving Eq

instance Show (FreqH Freq1) where
  show (HF x) = show x
  
type FreqFH = FreqH Freq1

freqH1 :: FreqH a -> a
freqH1 (HF x) = x

freqHSet1 :: a -> FreqH a
freqHSet1 = HF  

data FrequencyS a b = F11 a | F12 b | F2 a b deriving Eq

instance Show (FrequencyS FreqFH FreqFL) where
  show (F11 x) = mconcat [show x, " "]
  show (F12 y) = mconcat [show y, " "]
  show (F2 x y) = mconcat [show x, show y, " "]

type FrequencyS2 = FrequencyS FreqFH FreqFL

data Sinc a b c d = SC1 a b c d | SC2 a b d c | SC a b c d c deriving Eq

instance Show (Sinc Sinc1 Phase1 Sinc2 FrequencyS2) where
  show (SC1 x y z t) = mconcat ["sinc ", show x, show y, show z, show t]
  show (SC2 x y t z) = mconcat ["sinc ", show x, show y, show t, show z]
  show (SC x y z1 t z2) = mconcat ["sinc ", show x, show y, show z1, show t, show z2]

type Sinc4 = Sinc Sinc1 Phase1 Sinc2 FrequencyS2

sincC :: Sinc a b c d -> String
sincC (SC1 _ _ _ _) = "SC1"
sincC (SC2 _ _ _ _) = "SC2"
sincC (SC _ _ _ _ _) = "SC"

sinc1 :: Sinc a b c d -> a
sinc1 (SC1 x _ _ _) = x
sinc1 (SC2 x _ _ _) = x
sinc1 (SC x _ _ _ _) = x

sinc2 :: Sinc a b c d -> b
sinc2 (SC1 _ y _ _) = y
sinc2 (SC2 _ y _ _) = y
sinc2 (SC _ y _ _ _) = y

sinc3 :: Sinc a b c d -> One2 c
sinc3 (SC1 _ _ z _) = O21 z
sinc3 (SC2 _ _ _ z) = O21 z
sinc3 (SC _ _ z1 _ z2) = O22 z1 z2

sinc4 :: Sinc a b c d -> d
sinc4 (SC1 _ _ _ t) = t
sinc4 (SC2 _ _ t _) = t
sinc4 (SC _ _ _ t _) = t

sincSet1 :: a -> Sinc a b c d -> Sinc a b c d
sincSet1 x (SC1 _ y z t) = SC1 x y z t
sincSet1 x (SC2 _ y t z) = SC2 x y t z
sincSet1 x (SC _ y z1 t z2) = SC x y z1 t z2

sincSet2 :: b -> Sinc a b c d -> Sinc a b c d
sincSet2 y (SC1 x _ z t) = SC1 x y z t
sincSet2 y (SC2 x _ z t) = SC2 x y z t
sincSet2 y (SC x _ z1 t z2) = SC x y z1 t z2

sincSet3 :: One2 c -> Sinc a b c d -> Sinc a b c d
sincSet3 (O21 z) (SC1 x y _ t) = SC1 x y z t
sincSet3 (O22 z1 z2) (SC1 x y _ t) = SC x y z1 t z2
sincSet3 (O21 z) (SC2 x y t _) = SC2 x y t z
sincSet3 (O22 z1 z2) (SC2 x y t _) = SC x y z1 t z2
sincSet3 (O21 z) (SC x y _ t z2) = SC x y z t z2
sincSet3 (O22 z1 z2) (SC x y _ t _) = SC x y z1 t z2

sincSet4 :: d -> Sinc a b c d -> Sinc a b c d
sincSet4 t (SC1 x y z _) = SC1 x y z t
sincSet4 t (SC2 x y _ z) = SC2 x y t z
sincSet4 t (SC x y z1 _ z2) = SC x y z1 t z2

showSCQ :: Sinc4 -> [String]
showSCQ = words . show 
