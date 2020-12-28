-- |
-- Module      :  DobutokO.Sound.Effects.DCShift
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"dcshift\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.DCShift where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import DobutokO.Sound.ToRange
import Numeric (showFFloat)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data DCShift a b = DC1 a | DC2 a b deriving Eq

instance Show (DCShift Float Float) where
  show (DC1 x) = mconcat ["dcshift ", showFFloat Nothing (toRange 2.0 x) " "]
  show (DC2 x y) = mconcat ["dcshift ", showFFloat Nothing (toRange 2.0 x) " ", showFFloat Nothing (toRange 0.1 (abs y)) " "]

type DCSh = DCShift Float Float

dcShiftC :: DCShift a b -> String
dcShiftC (DC1 _) = "DC1"
dcShiftC _ = "DC2"

dcShift1 :: DCShift a b -> a
dcShift1 (DC1 x) = x
dcShift1 (DC2 x _) = x

dcShift2 :: DCShift a b -> Maybe b
dcShift2 (DC2 _ y) = Just y
dcShift2 _ = Nothing

dcShiftSet1 :: a -> DCShift a b -> DCShift a b
dcShiftSet1 x (DC1 _) = DC1 x
dcShiftSet1 x (DC2 _ y) = DC2 x y

dcShiftSet2 :: b -> DCShift a b -> DCShift a b
dcShiftSet2 y (DC1 x) = DC2 x y
dcShiftSet2 y (DC2 x _) = DC2 x y

showDCQ :: DCSh -> [String]
showDCQ = words . show 
