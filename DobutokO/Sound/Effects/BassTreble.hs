-- |
-- Module      :  DobutokO.Sound.Effects.BassTreble
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"bass\" or \"treble\" effects with the needed specifications. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.BassTreble where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.Effects.Specs hiding (Width(..),Width1)
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data WidthS a = H a | K a | O a | Q a | S a deriving Eq

instance Show (WidthS Float) where 
  show (H x) = showFFloat Nothing x "h"
  show (K x) = showFFloat Nothing x "k"
  show (O x) = showFFloat Nothing x "o"
  show (Q x) = showFFloat Nothing x "q"
  show (S x) = showFFloat Nothing x "s"

type WidthS1 = WidthS Float

data FreqWidthS a b = FrS1 a | FrWS2 a b deriving Eq

instance Show (FreqWidthS Freq1 WidthS1) where
  show (FrS1 x) = show x
  show (FrWS2 x y) = mconcat [show x," ",show y]

type FreqWS2 = FreqWidthS Freq1 WidthS1   

freqWidthSC :: FreqWidthS a b -> String
freqWidthSC (FrS1 _) = "FrS1"
freqWidthSC (FrWS2 _ _) = "FrWS2"

freqWidthS1 :: FreqWidthS a b -> a
freqWidthS1 (FrS1 x) = x
freqWidthS1 (FrWS2 x _) = x

freqWidthS2 :: FreqWidthS a b -> Maybe b
freqWidthS2 (FrS1 _) = Nothing
freqWidthS2 (FrWS2 _ y) = Just y

freqWidthSSet1 :: a -> FreqWidthS a b -> FreqWidthS a b
freqWidthSSet1 x (FrS1 _) = FrS1 x
freqWidthSSet1 x (FrWS2 _ y) = FrWS2 x y

freqWidthSSet2 :: b -> FreqWidthS a b -> FreqWidthS a b
freqWidthSSet2 y (FrS1 x) = FrWS2 x y
freqWidthSSet2 y (FrWS2 x _) = FrWS2 x y

data Bass a b = Bs a b deriving Eq

instance Show (Bass Float FreqWS2) where
  show (Bs x y) = mconcat ["bass ",showFFloat Nothing x " ",show y," "]

type Bass1 = Bass Float FreqWS2

bass1 :: Bass a b -> a
bass1 (Bs x _) = x

bass2 :: Bass a b -> b
bass2 (Bs _ y) = y

bassSet1 :: a -> Bass a b -> Bass a b
bassSet1 x (Bs _ y) = Bs x y

bassSet2 :: b -> Bass a b -> Bass a b
bassSet2 y (Bs x _) = Bs x y

showBsQ :: Bass1 -> [String]
showBsQ = words . show  

data Treble a b = Tr a b deriving Eq

instance Show (Treble Float FreqWS2) where
  show (Tr x y) = mconcat ["treble ",showFFloat Nothing x " ",show y," "]

type Treble1 = Treble Float FreqWS2

treble1 :: Treble a b -> a
treble1 (Tr x _) = x

treble2 :: Treble a b -> b
treble2 (Tr _ y) = y

trebleSet1 :: a -> Treble a b -> Treble a b
trebleSet1 x (Tr _ y) = Tr x y

trebleSet2 :: b -> Treble a b -> Treble a b
trebleSet2 y (Tr x _) = Tr x y

showTrQ :: Treble1 -> [String]
showTrQ = words . show  
