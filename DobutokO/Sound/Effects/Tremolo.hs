-- |
-- Module      :  DobutokO.Sound.Effects.Tremolo
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"tremolo\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Tremolo where

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

data Tremolo a = TL1 a | TL2 a a deriving Eq

instance Show (Tremolo Float) where
  show (TL1 x) = mconcat ["tremolo ", showFFloat Nothing (abs x) " "]
  show (TL2 x y) = mconcat ["tremolo ", showFFloat Nothing (abs x) " ", showFFloat Nothing (toRange 100.0 (abs y)) " "]
  
type Treml = Tremolo Float

tremoloC :: Tremolo a -> String
tremoloC (TL1 _) = "TL1"
tremoloC _ = "TL2"

tremolo1 :: Tremolo a -> a
tremolo1 (TL1 x) = x
tremolo1 (TL2 x _) = x

tremolo2 :: Tremolo a -> Maybe a
tremolo2 (TL2 _ y) = Just y
tremolo2 _ = Nothing

tremoloSet1 :: a -> Tremolo a -> Tremolo a
tremoloSet1 x (TL2 _ y) = TL2 x y
tremoloSet1 x _ = TL1 x

tremoloSet2 :: a -> Tremolo a -> Tremolo a
tremoloSet2 y (TL2 x _) = TL2 x y
tremoloSet2 y (TL1 x) = TL2 x y

showTLQ :: Treml -> [String]
showTLQ = words . show 
