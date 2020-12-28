-- |
-- Module      :  DobutokO.Sound.Effects.Spectrogram
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"spectrogram\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Spectrogram where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import qualified DobutokO.Sound.Effects.Timespec as TS
import DobutokO.Sound.Effects.Misc (MscS(..))

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data SFloat1 a = X1 a | X a | Y1 a | Y a | Z1 a | Z a | Q a | W a | P a deriving Eq

instance Show (SFloat1 Float) where
  show (X1 x) = mconcat ["-x ", showFFloat Nothing (if compare (toRange 200000.0 . abs $ x) 100.0 == LT then 100.0 else toRange 200000.0 . abs $ x) " "]
  show (X x) = mconcat ["-X ", showFFloat Nothing (if compare (toRange 5000.0 . abs $ x) 1.0 == LT then 1.0 else toRange 5000.0 . abs $ x) " "]
  show (Y1 x) = mconcat ["-y ", showFFloat Nothing (if compare (toRange 1200.0 . abs $ x) 64.0 == LT then 64.0 else toRange 1200.0 . abs $ x) " "]
  show (Y x) = mconcat ["-Y ", showFFloat Nothing (if compare (toRange 2050.0 . abs $ x) 130.0 == LT then 130.0 else toRange 2050.0 . abs $ x) " "]
  show (Z1 x) = mconcat ["-z ", showFFloat Nothing (if compare (toRange 180.0 . abs $ x) 20.0 == LT then 20.0 else toRange 180.0 . abs $ x) " "]
  show (Z x) = mconcat ["-Z ", showFFloat Nothing (toRange 100.0 x) " "]
  show (Q x) = mconcat ["-q ", showFFloat Nothing (toRange 249.0 . abs $ x) " "]
  show (W x) = mconcat ["-W ", showFFloat Nothing (toRange 10.0 x) " "]
  show (P x) = mconcat ["-p ", showFFloat Nothing (if compare (toRange 6.0 . abs $ x) 1.0 == LT then 1.0 else toRange 6.0 . abs $ x) " "]

type SFloat = SFloat1 Float

sFloat1C :: SFloat1 a -> String
sFloat1C (X1 _) = "X1"
sFloat1C (X _) = "X"
sFloat1C (Y1 _) = "Y1"
sFloat1C (Y _) = "Y"
sFloat1C (Z1 _) = "Z1"
sFloat1C (Z _) = "Z"
sFloat1C (Q _) = "Q"
sFloat1C (W _) = "W"
sFloat1C _ = "P"

sFloat11 :: SFloat1 a -> a
sFloat11 (X1 x) = x
sFloat11 (X x) = x
sFloat11 (Y1 x) = x
sFloat11 (Y x) = x
sFloat11 (Z1 x) = x
sFloat11 (Z x) = x
sFloat11 (Q x) = x
sFloat11 (W x) = x
sFloat11 (P x) = x

sFloat1Set1 :: a -> SFloat1 a -> SFloat1 a
sFloat1Set1 x (X1 _) = X1 x
sFloat1Set1 x (X _) = X x
sFloat1Set1 x (Y1 _) = Y1 x
sFloat1Set1 x (Y _) = Y x
sFloat1Set1 x (Z1 _) = Z1 x
sFloat1Set1 x (Z _) = Z x
sFloat1Set1 x (Q _) = Q x
sFloat1Set1 x (W _) = W x
sFloat1Set1 x _ = P x

data SString1 a = W1 a | T a | C a | O a deriving Eq

-- | For 'W1' the argument can be one of the following: \"Hann\" (default), \"Hamming\", \"Bartlett\", \"Rectangular\", \"Kaiser\", \"Dolph\".
instance Show (SString1 String) where
  show (W1 xs) 
    | null xs || take 3 xs == "Han" = []
    | head xs <= 'B' = "-w Bartlett "
    | head xs <= 'D' = "-w Dolph "
    | head xs <= 'H' = "-w Hamming "
    | head xs <= 'K' = "-w Kaiser "
    | head xs <= 'R' = "-w Rectangular "
    | otherwise = ""
  show (T xs) = mconcat ["-t ", xs , " "]
  show (C xs) = mconcat ["-c ", xs , " "]
  show (O xs) = mconcat ["-o ", xs , " "]

type SString = SString1 String

sString1C :: SString1 a -> String
sString1C (W1 _) = "W1"
sString1C (T _) = "T"
sString1C (C _) = "C"
sString1C _ = "O"

sString11 :: SString1 a -> a
sString11 (W1 x) = x
sString11 (T x) = x
sString11 (C x) = x
sString11 (O x) = x

sString1Set1 :: a -> SString1 a -> SString1 a
sString1Set1 x (W1 _) = W1 x
sString1Set1 x (T _) = T x
sString1Set1 x (C _) = C x
sString1Set1 x (O _) = O x

data Spectr = S1 | M | H | L | A1 | A | R deriving Eq

instance Show Spectr where
  show S1 = "-s "
  show M = "-m "
  show H = "-h "
  show L = "-l "
  show A1 = "-a "
  show A = "-A "
  show _ = "-r "

data Advanced1 a = S a deriving Eq

instance Show (Advanced1 TS.TSpecification) where
  show (S x) = mconcat ["-S ", show x]

advanced11 :: Advanced1 a -> a
advanced11 (S x) = x

advanced1Set1 :: a -> Advanced1 a
advanced1Set1 = S

type PositionS = Advanced1 TS.TSpecification  

data DTSpec2 a b = DTs b | DTm a b | DTh a a b | DS a deriving Eq

instance Show (DTSpec2 Int Float) where 
  show (DTs y) = showFFloat Nothing (abs y) "t"
  show (DTm y z) = mconcat [show (abs y),":",showFFloat Nothing (abs z) "t"]
  show (DTh y1 y2 z) = mconcat [show (abs y1),":",show (abs y2),":",showFFloat Nothing (abs z) "t"]
  show (DS y) = mconcat [show (abs y),"s"]

type FirstDTSpec = DTSpec2 Int Float

isTimeD :: DTSpec2 a b -> Bool
isTimeD (DS _) = False
isTimeD _ = True

isSamplesD :: DTSpec2 a b -> Bool
isSamplesD (DS _) = True
isSamplesD _ = False

dTSpec2CD :: FirstDTSpec -> String
dTSpec2CD (DTs _) = "DTs"
dTSpec2CD (DTm _ _) = "DTm"
dTSpec2CD (DTh _ _ _) = "DTh"
dTSpec2CD (DS _) = "DS"

secondsD :: FirstDTSpec -> Maybe Float
secondsD (DTs x) = Just (abs x)
secondsD (DTm x y) = Just (abs y + fromIntegral (60 * abs x))
secondsD (DTh x y z) = Just (abs z + fromIntegral (3600 * abs x + 60 * abs y))
secondsD _ = Nothing

minutesD :: FirstDTSpec -> Maybe Int
minutesD (DTs x) = Just (truncate $ abs x / 60.0)
minutesD (DTm x y) = Just (abs x + truncate (abs y / 60.0))
minutesD (DTh x y z) = Just (abs y + truncate (abs z / 60.0) + 60 * abs x)
minutesD _ = Nothing

hoursD :: FirstDTSpec -> Maybe Int
hoursD (DTs x) = Just (truncate $ abs x / 3600.0)
hoursD (DTm x y) = Just (truncate (fromIntegral (abs x) / 60.0 + abs y / 3600.0))
hoursD (DTh x y z) = Just (abs x + truncate (abs z / 3600.0 + fromIntegral (abs y) / 60.0))
hoursD _ = Nothing

samplesD :: FirstDTSpec -> Maybe Int
samplesD (DS x) = Just x
samplesD _ = Nothing

seconds2FstDTSpec2 :: Float -> FirstDTSpec
seconds2FstDTSpec2 y = DTs (abs y)

samples2FstDTSpec2 :: Int -> FirstDTSpec
samples2FstDTSpec2 y = DS (abs y)

type TSpec = TS.TimeSpec FirstDTSpec TS.NextTSpec

instance Show (TS.TimeSpec FirstDTSpec TS.NextTSpec) where
  show (TS.TS1 x) = show x
  show (TS.TS2 x ys) = mconcat [show x,mconcat . map show $ ys]

data DurationD1 a = D a deriving Eq

instance Show (DurationD1 TSpec) where 
  show (D x) = show x

type DurationD = DurationD1 TSpec

data Spectrogram3 a b c d e = SG [a] [b] [c] [d] [e] deriving Eq

instance Show (Spectrogram3 SFloat SString Spectr PositionS DurationD) where
  show (SG xs ys zs ts us) = mconcat ["spectrogram ", show (Msc xs), show (Msc ys), show (Msc zs), show (Msc ts), show (Msc us)]
  
type Spectrogram = Spectrogram3 SFloat SString Spectr PositionS DurationD

spectrogram31 :: Spectrogram3 a b c d e -> [a]
spectrogram31 (SG xs _ _ _ _) = xs

spectrogram32 :: Spectrogram3 a b c d e -> [b]
spectrogram32 (SG _ ys _ _ _) = ys

spectrogram33 :: Spectrogram3 a b c d e -> [c]
spectrogram33 (SG _ _ zs _ _) = zs

spectrogram34 :: Spectrogram3 a b c d e -> [d]
spectrogram34 (SG _ _ _ ts _) = ts

spectrogram35 :: Spectrogram3 a b c d e -> [e]
spectrogram35 (SG _ _ _ _ us) = us

spectrogramSet31 :: [a] -> Spectrogram3 a b c d e -> Spectrogram3 a b c d e
spectrogramSet31 xs (SG _ ys zs ts us) = SG xs ys zs ts us

spectrogramSet32 :: [b] -> Spectrogram3 a b c d e -> Spectrogram3 a b c d e
spectrogramSet32 ys (SG xs _ zs ts us) = SG xs ys zs ts us

spectrogramSet33 :: [c] -> Spectrogram3 a b c d e -> Spectrogram3 a b c d e
spectrogramSet33 zs (SG xs ys _ ts us) = SG xs ys zs ts us

spectrogramSet34 :: [d] -> Spectrogram3 a b c d e -> Spectrogram3 a b c d e
spectrogramSet34 ts (SG xs ys zs _ us) = SG xs ys zs ts us

spectrogramSet35 :: [e] -> Spectrogram3 a b c d e -> Spectrogram3 a b c d e
spectrogramSet35 us (SG xs ys zs ts _) = SG xs ys zs ts us

showSGQ :: Spectrogram -> [String]
showSGQ = words . show 
