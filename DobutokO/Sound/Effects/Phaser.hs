-- |
-- Module      :  DobutokO.Sound.Effects.Phaser
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"phaser\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Phaser where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import Numeric (showFFloat)
import DobutokO.Sound.Effects.Modulation2

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Phaser a b = Ph a a a a a b deriving Eq

instance Show (Phaser Float Modulation) where 
  show (Ph gainin gainout delay decay speed mod1) = mconcat ["phaser ",showFFloat Nothing gainin " ",showFFloat Nothing  gainout " ", showFFloat Nothing delay " ",
    showFFloat Nothing decay " ", showFFloat Nothing speed " ", show mod1]

type Phaser2 = Phaser Float Modulation

phaser1 :: Int -> Phaser a b -> a
phaser1 n (Ph x0 x1 x2 x3 x4 _) 
  | n == 1 = x0
  | n == 2 = x1
  | n == 3 = x2
  | n == 4 = x3
  | n == 5 = x4
  | otherwise = error "DobutokO.Sound.Effects.Phaser.phaser1: Not defined parameter. "
  
phaser2 :: Phaser a b -> b
phaser2 (Ph _ _ _ _ _ y) = y

phaserSet1 :: Int -> a -> Phaser a b -> Phaser a b
phaserSet1 n x (Ph x0 x1 x2 x3 x4 y) 
  | n == 1 = Ph x x1 x2 x3 x4 y
  | n == 2 = Ph x0 x x2 x3 x4 y
  | n == 3 = Ph x0 x1 x x3 x4 y
  | n == 4 = Ph x0 x1 x2 x x4 y
  | n == 5 = Ph x0 x1 x2 x3 x y
  | otherwise = error "DobutokO.Sound.Effects.Phaser.phaserSet1: Not defined parameter. "

phaserSet2 :: b -> Phaser a b -> Phaser a b
phaserSet2 y (Ph x0 x1 x2 x3 x4 _) = Ph x0 x1 x2 x3 x4 y

showPhQ :: Phaser2 -> [String]
showPhQ = words . show
