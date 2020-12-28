-- |
-- Module      :  DobutokO.Sound.Effects.Dither
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"dither\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Dither where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data NoiseType = Lipshitz | FWeighted | ModifiedEWeighted | ImprovedEWeighted | Gesemann | Shibata | LowShibata | HighShibata 
  deriving Eq
  
instance Show NoiseType where
  show Lipshitz = "lipshitz "
  show FWeighted = "f-weighted "
  show ModifiedEWeighted = "modified-e-weighted "
  show ImprovedEWeighted = "improved-e-weighted "
  show Gesemann = "gesemann "
  show Shibata = "shibata "
  show LowShibata = "low-shibata "
  show HighShibata = "high-shibata "

data Filter a = N | Ss | S | F a deriving Eq

instance Show (Filter NoiseType) where
  show N = ""
  show S = "-S "
  show (F x) = mconcat ["-f ", show x]
  show _ = "-s "

type FilterN = Filter NoiseType 

filterC :: Filter a -> String
filterC N = "N"
filterC S = "S"
filterC Ss = "Ss"
filterC _ = "F"

filter1 :: Filter a -> Maybe a
filter1 (F x) = Just x
filter1 _ = Nothing

filterN1 :: FilterN -> Maybe NoiseType
filterN1 (F x) = Just x
filterN1 Ss = Just Shibata
filterN1 _ = Nothing

filterSet1 :: a -> Filter a
filterSet1 = F

data AutoD = A | N0 deriving Eq

instance Show AutoD where
  show A = "-a "
  show _ = ""

autoDC :: AutoD -> String
autoDC A = "A"
autoDC _ ="N0"

data PrecisionD a = P a | N2 deriving Eq

instance Show (PrecisionD Float) where
  show N2 = ""
  show (P x) = mconcat ["-p ", showFFloat Nothing (if compare (toRange 24.0 . abs $ x) 1.0 == LT then 1.0 else (toRange 24.0 . abs $ x)) " "]

type Precision = PrecisionD Float

precisionDC :: PrecisionD a -> String
precisionDC (P _) = "P"
precisionDC _ = "N2"

precisionD1 :: PrecisionD a -> Maybe a
precisionD1 (P x) = Just x
precisionD1 _ = Nothing

precisionSet1 :: Float -> Precision
precisionSet1 x = if compare (toRange 24.0 . abs $ x) 1.0 == LT then P 1.0 else P (toRange 24.0 . abs $ x)

data Dither a b c = DT0 | DT100 a | DT010 b | DT001 c | DT011 b c | DT110 a b | DT101 a c | DT a b c deriving Eq

instance Show (Dither FilterN AutoD Precision) where
  show DT0 = "dither "
  show (DT100 x) = mconcat ["dither ", show x]
  show (DT010 y) = mconcat ["dither ", show y]
  show (DT001 z) = mconcat ["dither ", show z]
  show (DT011 y z) = mconcat ["dither ", show y, show z]
  show (DT110 x y) = mconcat ["dither ", show x, show y]
  show (DT101 x z) = mconcat ["dither ", show x, show z]
  show (DT x y z) = mconcat ["dither ", show x, show y, show z]

type Dith = Dither FilterN AutoD Precision

ditherC :: Dither a b c -> String
ditherC DT0 = "DT0"
ditherC (DT100 _) = "DT100"
ditherC (DT010 _) = "DT010"
ditherC (DT001 _) = "DT001"
ditherC (DT011 _ _) = "DT011"
ditherC (DT110 _ _) = "DT110"
ditherC (DT101 _ _) = "DT101"
ditherC _ = "DT"

dither1 :: Dither a b c -> Maybe a
dither1 (DT100 x) = Just x
dither1 (DT101 x _) = Just x
dither1 (DT110 x _) = Just x
dither1 (DT x _ _) = Just x
dither1 _ = Nothing

dither2 :: Dither a b c -> Maybe b
dither2 (DT010 y) = Just y
dither2 (DT011 y _) = Just y
dither2 (DT110 _ y) = Just y
dither2 (DT _ y _) = Just y
dither2 _ = Nothing

dither3 :: Dither a b c -> Maybe c
dither3 (DT001 z) = Just z
dither3 (DT101 _ z) = Just z
dither3 (DT011 _ z) = Just z
dither3 (DT _ _ z) = Just z
dither3 _ = Nothing

ditherSet1 :: a -> Dither a b c -> Dither a b c
ditherSet1 x (DT010 y) = DT110 x y
ditherSet1 x (DT001 z) = DT101 x z
ditherSet1 x (DT011 y z) = DT x y z
ditherSet1 x (DT110 _ y) = DT110 x y
ditherSet1 x (DT101 _ z) = DT101 x z
ditherSet1 x (DT _ y z) = DT x y z
ditherSet1 x _ = DT100 x

ditherSet2  :: b -> Dither a b c -> Dither a b c
ditherSet2  y (DT100 x) = DT110 x y
ditherSet2  y (DT001 z) = DT011 y z
ditherSet2  y (DT011 _ z) = DT011 y z
ditherSet2  y (DT110 x _) = DT110 x y
ditherSet2  y (DT101 x z) = DT x y z
ditherSet2  y (DT x _ z) = DT x y z
ditherSet2  y _ = DT010 y

ditherSet3  :: c -> Dither a b c -> Dither a b c
ditherSet3  z (DT100 x) = DT101 x z
ditherSet3  z (DT010 y) = DT011 y z
ditherSet3  z (DT011 y _) = DT011 y z
ditherSet3  z (DT101 x _) = DT101 x z
ditherSet3  z (DT110 x y) = DT x y z
ditherSet3  z (DT x y _) = DT x y z
ditherSet3  z _ = DT001 z

showDQ :: Dith -> [String]
showDQ = words . show 
