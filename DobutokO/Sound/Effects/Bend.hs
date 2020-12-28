-- |
-- Module      :  DobutokO.Sound.Effects.Bend
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"bend\" SoX effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Bend where

import Numeric (showFFloat)
import DobutokO.Sound.Effects.Timespec

data BendTrio a b = Bend3 a b a deriving Eq

instance Show (BendTrio FirstTSpec Float) where
  show (Bend3 x y z) = mconcat [show x, ",", showFFloat Nothing y ",", show z]

type BendTr3 = BendTrio FirstTSpec Float

bendTrio1 :: BendTrio a b -> a
bendTrio1 (Bend3 x _ _) = x

bendTrio2 :: BendTrio a b -> b
bendTrio2 (Bend3 _ y _) = y

bendTrio3 :: BendTrio a b -> a
bendTrio3 (Bend3 _ _ z) = z

bendTrioSet1 :: a -> BendTrio a b -> BendTrio a b
bendTrioSet1 x (Bend3 _ y z) = Bend3 x y z

bendTrioSet2 :: b -> BendTrio a b -> BendTrio a b
bendTrioSet2 y (Bend3 x _ z) = Bend3 x y z

bendTrioSet3 :: a -> BendTrio a b -> BendTrio a b
bendTrioSet3 z (Bend3 x y _) = Bend3 x y z

data FrameRate a = FR a deriving Eq

instance Show (FrameRate Float) where
  show (FR x) 
    | compare x 10.0 /= LT && compare x 80.0 /= GT = "-f " ++ showFFloat Nothing x " "
    | otherwise = ""

type FrRate = FrameRate Float    

frameRate1 :: FrameRate a -> a
frameRate1 (FR x) = x

frameRateSet1 :: a -> FrameRate a
frameRateSet1 = FR

data OverSample a = OS a deriving Eq

instance Show (OverSample Float) where
  show (OS x) 
    | compare x 4.0 /= LT && compare x 32.0 /= GT = "-o " ++ showFFloat Nothing x " "
    | otherwise = ""

type OvSample = OverSample Float

overSample1 :: OverSample a -> a
overSample1 (OS x) = x

overSampleSet1 :: a -> OverSample a
overSampleSet1 = OS

data Bend a b c = Bnd c | Bnd1 a c | Bnd2 b c | Bnd12 a b c deriving Eq

instance Show (Bend FrRate OvSample BendTr3) where
  show (Bnd z) = mconcat ["bend ",show z]
  show (Bnd1 x z) = mconcat ["bend ",show x,show z]
  show (Bnd2 y z) = mconcat ["bend ",show y,show z]
  show (Bnd12 x y z) = mconcat ["bend ", show x, show y, show z]

type BendE = Bend FrRate OvSample BendTr3

bend1 :: Bend a b c -> Maybe a
bend1 (Bnd1 x _) = Just x
bend1 (Bnd12 x _ _) = Just x
bend1 _ = Nothing

bend2 :: Bend a b c -> Maybe b
bend2 (Bnd2 y _) = Just y
bend2 (Bnd12 _ y _) = Just y
bend2 _ = Nothing

bendE1 :: BendE -> FrRate
bendE1 (Bnd1 x _) = x
bendE1 (Bnd12 x _ _) = x
bendE1 _ = FR 25.0

bendE2 :: BendE -> OvSample
bendE2 (Bnd2 y _) = y
bendE2 (Bnd12 _ y _) = y
bendE2 _ = OS 16.0

bend3 :: Bend a b c -> c
bend3 (Bnd z) = z
bend3 (Bnd1 _ z) = z
bend3 (Bnd2 _ z) = z
bend3 (Bnd12 _ _ z) = z

bendSet1 :: a -> Bend a b c -> Bend a b c
bendSet1 _ (Bnd z) = Bnd z
bendSet1 x (Bnd1 _ z) = Bnd1 x z
bendSet1 x (Bnd2 y z) = Bnd12 x y z
bendSet1 x (Bnd12 _ y z) = Bnd12 x y z

bendSet2 :: b -> Bend a b c -> Bend a b c
bendSet2 y (Bnd z) = Bnd2 y z
bendSet2 y (Bnd1 x z) = Bnd12 x y z
bendSet2 y (Bnd2 _ z) = Bnd2 y z
bendSet2 y (Bnd12 x _ z) = Bnd12 x y z

bendSet3 :: c -> Bend a b c -> Bend a b c
bendSet3 z (Bnd _) = Bnd z
bendSet3 z (Bnd1 x _) = Bnd1 x z
bendSet3 z (Bnd2 y _) = Bnd2 y z
bendSet3 z (Bnd12 x y _) = Bnd12 x y z

showBndQ :: BendE -> [String]
showBndQ = words . show
