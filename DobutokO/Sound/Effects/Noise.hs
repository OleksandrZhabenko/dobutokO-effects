-- |
-- Module      :  DobutokO.Sound.Effects.Noise
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"noiseprof\" and \"noisered\" effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Noise where

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

data Noiseprof a = N | NP a deriving Eq

instance Show (Noiseprof FilePath) where
  show (NP file) = mconcat ["noiseprof ",file, " "]
  show _ = "noiseprof "

type NoiseP = Noiseprof FilePath

noiseprofC :: Noiseprof a -> String
noiseprofC N = "N"
noiseprofC _ = "NP"

noiseprof1 :: Noiseprof a -> Maybe a
noiseprof1 (NP x) = Just x
noiseprof1 _ = Nothing

noiseprofSet1 :: a -> Noiseprof a
noiseprofSet1 = NP

showNPQ :: NoiseP -> [String]
showNPQ = words . show 

data Noisered a b = NR | NR1 a | NR2 a b deriving Eq

instance Show (Noisered FilePath Float) where
  show (NR2 file x) = mconcat ["noisered ",show file, " ", showFFloat Nothing (toRange 1.0 . abs $ x) " "]
  show (NR1 file) = mconcat ["noisered ",show file, " "]
  show _ = "noisered - " -- the shell command will expect input from stdin. If it is not prepared, planned and available, do not use at all.

type NoiseR = Noisered FilePath Float

noiseredC :: Noisered a b -> String
noiseredC (NR2 _ _) = "NR2"
noiseredC (NR1 _) = "NR1"
noiseredC _ = "NR"

noisered1 :: Noisered a b -> Maybe a
noisered1 (NR2 x _) = Just x
noisered1 (NR1 x) = Just x
noisered1 _ = Nothing

noisered2 :: Noisered a b -> Maybe b
noisered2 (NR2 _ y) = Just y
noisered2 _ = Nothing

noiseredSet1 :: a -> Noisered a b -> Noisered a b
noiseredSet1 x (NR2 _ y) = NR2 x y
noiseredSet1 x _ = NR1 x

noiseredSet2 :: b -> Noisered a b -> Maybe (Noisered a b)
noiseredSet2 y (NR2 x _) = Just (NR2 x y)
noiseredSet2 y (NR1 x) = Just (NR2 x y)
noiseredSet2 _ _ = Nothing

showNRQ :: NoiseR -> [String]
showNRQ = words . show 
