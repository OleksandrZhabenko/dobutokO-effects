-- |
-- Module      :  DobutokO.Sound.Effects.Overdrive
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"overdrive\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Overdrive where

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

data Overdrive a = OD | OD1 a | OD2 a a deriving Eq

instance Show (Overdrive Float) where
  show (OD1 x) = mconcat ["overdrive ", showFFloat Nothing (toRange 100.0 (abs x)) " "]
  show (OD2 x y) = mconcat ["overdrive ", showFFloat Nothing (toRange 100.0 (abs x)) " ", showFFloat Nothing (toRange 100.0 (abs y)) " "]
  show _ = "overdrive "

type Ovdrive = Overdrive Float

overdriveC :: Overdrive a -> String
overdriveC OD = "OD"
overdriveC (OD1 _) = "OD1"
overdriveC _ = "OD2"

overdrive1 :: Overdrive a -> Maybe a
overdrive1 (OD1 x) = Just x
overdrive1 (OD2 x _) = Just x
overdrive1 _ = Nothing

overdrive2 :: Overdrive a -> Maybe a
overdrive2 (OD2 _ y) = Just y
overdrive2 _ = Nothing

overdriveSet1 :: a -> Overdrive a -> Overdrive a
overdriveSet1 x (OD2 _ y) = OD2 x y
overdriveSet1 x _ = OD1 x

overdriveSet2 :: a -> Overdrive a -> Maybe (Overdrive a)
overdriveSet2 y (OD2 x _) = Just (OD2 x y)
overdriveSet2 y (OD1 x) = Just (OD2 x y)
overdriveSet2 _ _ = Nothing

showODQ :: Ovdrive -> [String]
showODQ = words . show 
