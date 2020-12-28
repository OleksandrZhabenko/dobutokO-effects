-- |
-- Module      :  DobutokO.Sound.Effects.Chorus
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"chorus\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Chorus where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import Numeric (showFFloat)
import DobutokO.Sound.Effects.Modulation2
import Data.List (intersperse)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data ChorusTail a b = ChT a a a a b deriving Eq

instance Show (ChorusTail Float Modulation) where
  show (ChT delay decay speed depth mod1) = mconcat [showFFloat Nothing (abs delay) " ", showFFloat Nothing (abs decay) " ", showFFloat Nothing (abs speed) " ", 
    showFFloat Nothing (abs depth) " ", show mod1]

type ChorusTail1 = ChorusTail Float Modulation

data Chorus a b = Ch a a [b] deriving Eq

instance Show (Chorus Float ChorusTail1) where
  show (Ch gin gout ys) 
   | null ys = ""
   | otherwise = mconcat ["chorus ", showFFloat Nothing (abs gin) " ", showFFloat Nothing (abs gout) " ", mconcat . intersperse " " . map show $ ys]

type Chorus1 = Chorus Float ChorusTail1  

chorusTail1 :: Int -> ChorusTail a b -> a
chorusTail1 n (ChT x0 x1 x2 x3 _) 
  | n == 1 = x0
  | n == 2 = x1
  | n == 3 = x2
  | n == 4 = x3
  | otherwise = error "DobutokO.Sound.Effects.Chorus.chorusTail1: Not defined parameter. "
  
chorusTail2 :: ChorusTail a b -> b
chorusTail2 (ChT _ _ _ _ y) = y

chorusTailSet1 :: Int -> a -> ChorusTail a b -> ChorusTail a b
chorusTailSet1 n x (ChT x0 x1 x2 x3 y) 
  | n == 1 = ChT x x1 x2 x3 y
  | n == 2 = ChT x0 x x2 x3 y
  | n == 3 = ChT x0 x1 x x3 y
  | n == 4 = ChT x0 x1 x2 x y
  | otherwise = error "DobutokO.Sound.Effects.Chorus.chorusTailSet1: Not defined parameter. "

chorusTailSet2 :: b -> ChorusTail a b -> ChorusTail a b
chorusTailSet2 y (ChT x0 x1 x2 x3 _) = ChT x0 x1 x2 x3 y

chorus1 :: Int -> Chorus a b -> a
chorus1 n (Ch x0 x1 _) 
  | n == 1 = x0
  | n == 2 = x1
  | otherwise = error "DobutokO.Sound.Effects.Chorus.chorus1: Not defined parameter. "
  
chorus2 :: Chorus a b -> [b]
chorus2 (Ch _ _ ys) = ys

chorusSet1 :: Int -> a -> Chorus a b -> Chorus a b
chorusSet1 n x (Ch x0 x1 y) 
  | n == 1 = Ch x x1 y
  | n == 2 = Ch x0 x y
  | otherwise = error "DobutokO.Sound.Effects.Chorus.chorusSet1: Not defined parameter. "

chorusSet2 :: [b] -> Chorus a b -> Chorus a b
chorusSet2 ys (Ch x0 x1 _) = Ch x0 x1 ys

showChQ :: Chorus1 -> [String]
showChQ = words . show
