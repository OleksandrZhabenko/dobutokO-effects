-- |
-- Module      :  DobutokO.Sound.Effects.MCompand
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"compand\" and \"mcompand\" effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.MCompand where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import Data.List (intersperse)
import DobutokO.Sound.One

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data FloatE a = InfP | InfM | Float1 a deriving Eq

instance Show (FloatE Float) where
  show InfP = "inf "
  show InfM = "-inf "
  show (Float1 x) = showFFloat Nothing x " "

type Float3 = FloatE Float

floatE1 :: FloatE a -> Maybe a
floatE1 (Float1 x) = Just x
floatE1 _ = Nothing

floatESet1 :: a -> FloatE a
floatESet1 = Float1 

isInfS :: FloatE a -> Bool
isInfS (Float1 _)  = False
isInfS _ = True

isInfP :: FloatE a -> Bool
isInfP InfP = True
isInfP _ = False

isInfM :: FloatE a -> Bool
isInfM InfM = True
isInfM _ = False

isFloatE1 :: FloatE a -> Bool
isFloatE1 (Float1 _) = True
isFloatE1 _ = False

absEP :: Float3 -> Float3
absEP (Float1 x) = Float1 (abs x)
absEP _ = InfP

absEN :: Float3 -> Float3
absEN (Float1 x) = Float1 (-abs x)
absEN _ = InfM

data CompandTail a b = N | CT1 a | CT2 a a | CT3 a a b deriving Eq

instance Show (CompandTail Float3 Float) where
  show N = " "
  show (CT1 x) = mconcat [show x, " "]
  show (CT2 x y) = mconcat [show x, " ", show (absEN y), " "]
  show (CT3 x y z) = mconcat [show x, " ", show (absEN y), " ",  showFFloat Nothing (abs z) " "]

type CompTail = CompandTail Float3 Float

compandTail1 :: CompandTail a b -> Maybe (One2 a)
compandTail1 (CT1 x) = Just (O21 x)
compandTail1 (CT2 x y) = Just (O22 x y)
compandTail1 (CT3 x y _) = Just (O22 x y)
compandTail1 _ = Nothing

compandTail2 :: CompandTail a b -> Maybe b
compandTail2 (CT3 _ _ y) = Just y
compandTail2 _ = Nothing

compandTailSet1 :: One2 a -> CompandTail a b -> CompandTail a b
compandTailSet1 (O21 x) (CT2 _ y) = CT2 x y
compandTailSet1 (O21 x) (CT3 _ y z) = CT3 x y z
compandTailSet1 (O22 x y) (CT2 _ _) = CT2 x y
compandTailSet1 (O22 x y) (CT3 _ _ z) = CT3 x y z
compandTailSet1 (O21 x) _ = CT1 x
compandTailSet1 (O22 x y) _ = CT2 x y

compandTailSet2 :: b -> CompandTail a b -> Maybe (CompandTail a b)
compandTailSet2 z (CT2 x y) = Just (CT3 x y z)
compandTailSet2 z (CT3 x y _) = Just (CT3 x y z)
compandTailSet2 _ _ = Nothing

showCTQ :: CompTail -> [String]
showCTQ = words . show

data Pair a = AD a a deriving Eq

instance Show (Pair Float) where
  show (AD x y) = mconcat [showFFloat Nothing (abs x) ",",showFFloat Nothing (abs y) ""]

type Pr = Pair Float  

pair1 :: Pair a -> a
pair1 (AD x _) = x

pair2 :: Pair a -> a
pair2 (AD _ y) = y

pairSet1 :: a -> Pair a -> Pair a
pairSet1 x (AD _ y) = AD x y

pairSet2 :: a -> Pair a -> Pair a
pairSet2 y (AD x _) = AD x y

data AtDe a = ADM a [a] deriving Eq

instance Show (AtDe Pr) where
  show (ADM (AD x y) zs) 
    | null zs = mconcat [show (AD x y), " "]
    | otherwise = mconcat [show (AD x y), ",", mconcat . intersperse "," . map show $ zs, " "]

type AtD2 = AtDe Pr

atDe1 :: AtDe a -> a
atDe1 (ADM x _) = x

atDe2 :: AtDe a -> [a]
atDe2 (ADM _ xs) = xs

atDeSet1 :: a -> AtDe a -> AtDe a
atDeSet1 x (ADM _ xs) = ADM x xs

atDeSet2 :: [a] -> AtDe a -> AtDe a
atDeSet2 xs (ADM x _) = ADM x xs

showADQ :: AtD2 -> [String]
showADQ = words . show 

data Neg a = NG a deriving Eq

instance Show (Neg Float) where
  show (NG x) = showFFloat Nothing (-abs x) " "

type Ng1 = Neg Float

neg1 :: Neg a -> a
neg1 (NG x) = x

negSet1 :: a -> Neg a
negSet1 = NG

type AtDeNF = AtDe (Neg Float)

instance Show AtDeNF where
  show (ADM (NG x) zs) 
    | null zs = mconcat [show (NG x), " "]
    | otherwise = mconcat [show (NG x), ",", mconcat . intersperse "," . map show $ zs, " "]

data SoftKnee a = NK | SK a deriving Eq

instance Show (SoftKnee Float) where
  show (SK x) = showFFloat Nothing x ":"
  show _ = ""

type SoftK1 = SoftKnee Float 

softKneeC :: SoftKnee a -> String
softKneeC NK = "NK"
softKneeC _ = "SK"

softKnee1 :: SoftKnee a -> Maybe a
softKnee1 (SK x)   = Just x
softKnee1 _ = Nothing

softKneeSet1 :: a -> SoftKnee a
softKneeSet1 = SK

data Compand a b c d = CP3 a b c | CP4 a b c d deriving Eq

instance Show (Compand AtD2 SoftK1 AtDeNF CompTail) where
  show (CP3 x y z) = mconcat ["compand ", show x, show y, show z]
  show (CP4 x y z t) = mconcat ["compand ", show x, show y, show z, show t]

type Compand4 = Compand AtD2 SoftK1 AtDeNF CompTail

compand1 :: Compand a b c d -> a
compand1 (CP3 x _ _) = x
compand1 (CP4 x _ _ _) = x

compand2 :: Compand a b c d -> b
compand2 (CP3 _ y _) = y
compand2 (CP4 _ y _ _) = y

compand3 :: Compand a b c d -> c
compand3 (CP3 _ _ z) = z
compand3 (CP4 _ _ z _) = z

compand4 :: Compand a b c d -> Maybe d
compand4 (CP4 _ _ _ t) = Just t
compand4 _ = Nothing

compandSet1 :: a -> Compand a b c d -> Compand a b c d
compandSet1 x (CP3 _ y z) = CP3 x y z
compandSet1 x (CP4 _ y z t) = CP4 x y z t

compandSet2 :: b -> Compand a b c d -> Compand a b c d
compandSet2 y (CP3 x _ z) = CP3 x y z
compandSet2 y (CP4 x _ z t) = CP4 x y z t

compandSet3 :: c -> Compand a b c d -> Compand a b c d
compandSet3 z (CP3 x y _) = CP3 x y z
compandSet3 z (CP4 x y _ t) = CP4 x y z t

compandSet4 :: d -> Compand a b c d -> Compand a b c d
compandSet4 t (CP3 x y z) = CP4 x y z t
compandSet4 t (CP4 x y z _) = CP4 x y z t

showCMPDQ :: Compand4 -> [String]
showCMPDQ = words . show 

data KFreq a = Fr a | KFr a deriving Eq

instance Show (KFreq Int) where
  show (Fr n) = mconcat [show (abs n), " "]
  show (KFr n) = mconcat [show (abs n), "k "]

instance Show (KFreq Float) where
  show (Fr x) = mconcat [showFFloat Nothing (abs x) " "]
  show (KFr x) = mconcat [showFFloat Nothing (abs x) "k "]  

type KFQ = KFreq Int

kFreqC :: KFQ -> String
kFreqC (Fr _)   = "Fr"
kFreqC _ = "KFr"

kFreq1 :: KFQ -> Int
kFreq1 (Fr n) = n
kFreq1 (KFr n) = fromIntegral 1000 * n

kFreqSet1 :: Int -> KFQ -> KFreq Float
kFreqSet1 n (Fr _) = Fr (fromIntegral n)
kFreqSet1 n _ = KFr (fromIntegral n / 1000.0)

data FreqComp a b = FrCmp a b deriving Eq

instance Show (FreqComp KFQ String) where 
  show (FrCmp x ys) = mconcat [show x, show ys, " "]

type FrCmpnd2 = FreqComp KFQ String

freqComp1 :: FreqComp a b -> a
freqComp1 (FrCmp x _) = x

freqComp2 :: FreqComp a b -> b
freqComp2 (FrCmp _ y) = y

freqCompSet1 :: a -> FreqComp a b -> FreqComp a b
freqCompSet1 x (FrCmp _ y) = FrCmp x y

freqCompSet2 :: b -> FreqComp a b -> FreqComp a b
freqCompSet2 y (FrCmp x _) = FrCmp x y

showFC :: FrCmpnd2 -> [String]
showFC = words . show 

data MCompand a b = MCN1 a | MCNM a [b] deriving Eq

instance Show (MCompand String FrCmpnd2) where 
  show (MCN1 xs) = mconcat ["mcompand ", show xs, " "]
  show (MCNM xs ys) = mconcat ["mcompand ", show xs, " ", mconcat . map show $ ys]

type MComPand2 = MCompand String FrCmpnd2

mCompandC :: MCompand a b -> String
mCompandC (MCN1 _) = "MCN1"
mCompandC (MCNM _ _) = "MCNM"

mCompand1 :: MCompand a b -> a
mCompand1 (MCN1 x) = x
mCompand1 (MCNM x _) = x

mCompand2 :: MCompand a b -> Maybe [b]
mCompand2 (MCNM _ ys) = Just ys
mCompand2 _ = Nothing

mCompandSet1 :: a -> MCompand a b -> MCompand a b
mCompandSet1 x (MCN1 _) = MCN1 x
mCompandSet1 x (MCNM _ ys) = MCNM x ys

mCompandSet2 :: [b] -> MCompand a b -> MCompand a b
mCompandSet2 ys (MCN1 x) = MCNM x ys
mCompandSet2 ys (MCNM x _) = MCNM x ys
