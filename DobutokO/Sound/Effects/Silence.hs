-- |
-- Module      :  DobutokO.Sound.Effects.Silence
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"silence\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}

module DobutokO.Sound.Effects.Silence where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import DobutokO.Sound.Effects.Timespec (TimeSpec(..),NextTSpec)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data LeftIntact = L | Nl deriving Eq

instance Show LeftIntact where
  show L = "-l "
  show _ = ""

data Threshold a = T1 a | D1 a | P1 a deriving Eq

instance Show (Threshold Float) where
  show (T1 x) = mconcat [show (truncate . abs $ x), " "]
  show (P1 x) = showFFloat Nothing (toRange 100.0 . abs $ x) "% "
  show (D1 x) = showFFloat Nothing (if x == 0.0 then -0.01 else -abs x) "d "

type Threshold1 = Threshold Float

thresholdC :: Threshold a -> String
thresholdC (T1 _) = "T1"
thresholdC (D1 _) = "D1"
thresholdC _ = "P1"

threshold1 :: Threshold a -> a
threshold1 (T1 x) = x
threshold1 (D1 x) = x
threshold1 (P1 x) = x

thresholdSet1 :: a -> Threshold a -> Threshold a
thresholdSet1 x (T1 _) = T1 x
thresholdSet1 x (D1 _) = D1 x
thresholdSet1 x (P1 _) = P1 x

data Duration a b = B a | T2 b | M a b deriving Eq -- there is a not clearly documented possibility to specify also hours as duration, but it is rarely used and so is omitted.

instance Show (Duration Int Float) where
  show (B n) = mconcat [show (abs n), " "]
  show (T2 x) = showFFloat Nothing (abs x) "t "
  show (M n x) = mconcat [show (abs n), ":", showFFloat Nothing (abs x) " "]

type Duration2 = Duration Int Float

durationC :: Duration a b -> String
durationC (B _) = "B"
durationC (T2 _) = "T2"
durationC _ = "M"

duration1 :: Duration a b -> Maybe a
duration1 (B x) = Just x
duration1 (M x _) = Just x
duration1 _ = Nothing

duration2 :: Duration a b -> Maybe b
duration2 (T2 y) = Just y
duration2 (M _ y) = Just y
duration2 _ = Nothing

durationSet :: a -> b -> Int -> Duration a b
durationSet x y n
 | n == 1 = B x
 | n == 2 = T2 y
 | otherwise = M x y

durationSet1d :: a -> Duration a b -> Duration a b
durationSet1d x (B _) = B x
durationSet1d x (T2 y) = M x y
durationSet1d x (M _ y) = M x y

durationSet2d :: b -> Duration a b -> Duration a b
durationSet2d y (B x) = M x y
durationSet2d y (T2 _) = T2 y
durationSet2d y (M x _) = M x y

-- | Analogical to 'TSpec' but without the first argument (it is unneeded here).
data STSpec a b = STs b | STm a b | STh a a b | SS a deriving Eq

instance Show (STSpec Int Float) where 
  show (STs y) = showFFloat Nothing (abs y) "t"
  show (STm y z) = mconcat [show (abs y),":",showFFloat Nothing (abs z) "t"]
  show (STh y1 y2 z) = mconcat [show (abs y1),":",show (abs y2),":",showFFloat Nothing (abs z) "t"] -- is rarely used, but is technically possible.
  show (SS y) = mconcat [show (abs y),"s"]

type Above1TSpec = STSpec Int Float

instance Show (TimeSpec Above1TSpec NextTSpec) where
  show (TS1 x) = mconcat [show x, " "]
  show (TS2 x ys) = mconcat [show x,mconcat . map show $ ys, " "]

type STSpecification1 = TimeSpec Above1TSpec NextTSpec

data STSpec2 a b = STs2 b | STm2 a b | STh2 a a b | SS2 a deriving Eq

instance Show (STSpec2 Int Float) where 
  show (STs2 y) = showFFloat Nothing y "t"
  show (STm2 y z) = mconcat [show y,":",showFFloat Nothing (abs z) "t"]
  show (STh2 y1 y2 z) = mconcat [show y1,":",show (abs y2),":",showFFloat Nothing (abs z) "t"] -- is rarely used, but is technically possible.
  show (SS2 y) = mconcat [show y,"s"]

type BelowTSpec = STSpec2 Int Float

instance Show (TimeSpec BelowTSpec NextTSpec) where
  show (TS1 x) = mconcat [show x, " "]
  show (TS2 x ys) = mconcat [show x,mconcat . map show $ ys, " "]

type STSpecification2 = TimeSpec BelowTSpec NextTSpec

data AboveTSpec1 a b c = Z | A a b c deriving Eq

instance Show (AboveTSpec1 STSpecification1 Duration2 Threshold1) where
  show (A x y z) = mconcat [show x, show y, show z]
  show _ = "0 "

type ATSpec = AboveTSpec1 STSpecification1 Duration2 Threshold1

aboveTSpec1 :: AboveTSpec1 a b c -> Maybe a
aboveTSpec1 (A x _ _) = Just x
aboveTSpec1 _ = Nothing

aboveTSpec2 :: AboveTSpec1 a b c -> Maybe b
aboveTSpec2 (A _ y _) = Just y
aboveTSpec2 _ = Nothing

aboveTSpec3 :: AboveTSpec1 a b c -> Maybe c
aboveTSpec3 (A _ _ z) = Just z
aboveTSpec3 _ = Nothing

aboveTSpecSet1 :: a -> b -> c -> AboveTSpec1 a b c
aboveTSpecSet1 = A

aboveTSpecSet1a :: a -> AboveTSpec1 a b c -> AboveTSpec1 a b c
aboveTSpecSet1a x (A _ y z) = A x y z
aboveTSpecSet1a _ _ = Z

aboveTSpecSet2a :: b -> AboveTSpec1 a b c -> AboveTSpec1 a b c
aboveTSpecSet2a y (A x _ z) = A x y z
aboveTSpecSet2a _ _ = Z

aboveTSpecSet3a :: c -> AboveTSpec1 a b c -> AboveTSpec1 a b c
aboveTSpecSet3a z (A x y _) = A x y z
aboveTSpecSet3a _ _ = Z

data BelowTSpec1 a b c = Z2 | BL a b c deriving Eq

instance Show (BelowTSpec1 STSpecification2 Duration2 Threshold1) where
  show (BL x y z) = mconcat [show x, show y, show z]
  show _ = ""

type BTSpec = BelowTSpec1 STSpecification2 Duration2 Threshold1

belowTSpec1 :: BelowTSpec1 a b c -> Maybe a
belowTSpec1 (BL x _ _) = Just x
belowTSpec1 _ = Nothing

belowTSpec2 :: BelowTSpec1 a b c -> Maybe b
belowTSpec2 (BL _ y _) = Just y
belowTSpec2 _ = Nothing

belowTSpec3 :: BelowTSpec1 a b c -> Maybe c
belowTSpec3 (BL _ _ z) = Just z
belowTSpec3 _ = Nothing

belowTSpecSet1 :: a -> b -> c -> BelowTSpec1 a b c
belowTSpecSet1 = BL

belowTSpecSet1b :: a -> BelowTSpec1 a b c -> BelowTSpec1 a b c
belowTSpecSet1b x (BL _ y z) = BL x y z
belowTSpecSet1b _ _ = Z2

belowTSpecSet2b :: b -> BelowTSpec1 a b c -> BelowTSpec1 a b c
belowTSpecSet2b y (BL x _ z) = BL x y z
belowTSpecSet2b _ _ = Z2

belowTSpecSet3b :: c -> BelowTSpec1 a b c -> BelowTSpec1 a b c
belowTSpecSet3b z (BL x y _) = BL x y z
belowTSpecSet3b _ _ = Z2

data Silence a b c = SL2 a b | SL3 a b c deriving Eq

instance Show (Silence LeftIntact ATSpec BTSpec) where 
  show (SL2 x y) = mconcat ["silence ", show x, show y]
  show (SL3 x y z) = mconcat ["silence ", show x, show y, show z]

type Silence3 = Silence LeftIntact ATSpec BTSpec

silenceC :: Silence a b c -> String
silenceC (SL2 _ _) = "SL2"
silenceC (SL3 _ _ _) = "SL3"

silence1 :: Silence a b c -> a
silence1 (SL2 x _) = x
silence1 (SL3 x _ _) = x

silence2 :: Silence a b c -> b
silence2 (SL2 _ y) = y
silence2 (SL3 _ y _) = y

silence3 :: Silence a b c -> Maybe c
silence3 (SL3 _ _ z) = Just z
silence3 _ = Nothing

silenceSet1 :: a -> Silence a b c -> Silence a b c
silenceSet1 x (SL2 _ y) = SL2 x y
silenceSet1 x (SL3 _ y z) = SL3 x y z

silenceSet2 :: b -> Silence a b c -> Silence a b c
silenceSet2 y (SL2 x _) = SL2 x y
silenceSet2 y (SL3 x _ z) = SL3 x y z

silenceSet3 :: c -> Silence a b c -> Silence a b c
silenceSet3 z (SL2 x y) = SL3 x y z
silenceSet3 z (SL3 x y _) = SL3 x y z

showSLQ :: Silence3 -> [String]
showSLQ = words . show 
