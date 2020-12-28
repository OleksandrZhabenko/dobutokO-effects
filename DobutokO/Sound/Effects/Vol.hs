-- |
-- Module      :  DobutokO.Sound.Effects.Vol
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"vol\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Vol where

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

data VolType = N | A | P | D deriving Eq

instance Show VolType where
  show A = "amplitude "
  show P = "power "
  show D = "dB "
  show _ = ""

data Vol2 a b = V1 a | V2 a b | V3 a b a deriving Eq

instance Show (Vol2 Float VolType) where
  show (V1 x) = mconcat ["vol ", showFFloat Nothing x " "]
  show (V2 x y) = mconcat ["vol ", showFFloat Nothing x " ", show y]
  show (V3 x y z) = mconcat ["vol ", showFFloat Nothing x " ", show y, showFFloat Nothing (toRange 0.1 (abs z)) " "]

type Vol = Vol2 Float VolType

volC :: Vol2 a b -> String
volC (V1 _) = "V1"
volC (V2 _ _) = "V2"
volC (V3 _ _ _) = "V3"

vol1 :: Vol2 a b -> a
vol1 (V1 x) = x
vol1 (V2 x _) = x
vol1 (V3 x _ _) = x

vol2 :: Vol2 a b -> Maybe b
vol2 (V2 _ y) = Just y
vol2 (V3 _ y _) = Just y
vol2 _ = Nothing

vol3 :: Vol2 a b -> Maybe a
vol3 (V3 _ _ z) = Just z
vol3 _ = Nothing

volSet1 :: a -> Vol2 a b -> Vol2 a b
volSet1 x (V1 _) = V1 x
volSet1 x (V2 _ y) = V2 x y
volSet1 x (V3 _ y z) = V3 x y z

volSet2 :: b -> Vol2 a b -> Vol2 a b
volSet2 y (V1 x) = V2 x y
volSet2 y (V2 x _) = V2 x y
volSet2 y (V3 x _ z) = V3 x y z

volSet3 :: Float -> Vol -> Vol
volSet3 x (V1 x1) = V3 x1 N x
volSet3 x (V2 x1 y) = V3 x1 y x
volSet3 x (V3 x1 y _) = V3 x1 y x

showVQ :: Vol -> [String]
showVQ = words . show 
