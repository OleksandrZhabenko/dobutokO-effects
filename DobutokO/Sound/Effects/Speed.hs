-- |
-- Module      :  DobutokO.Sound.Effects.Speed
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"speed\" SoX effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Speed where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import Numeric (showFFloat)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Cents = E | C deriving Eq

instance Show Cents where
  show C = "c"
  show _ = ""
  
data Speed a b = Spd a b deriving Eq

instance Show (Speed Float Cents) where
  show (Spd x y) = mconcat [showFFloat Nothing x "",show y]

type Spd2 = Speed Float Cents

speed1 :: Speed a b -> a
speed1 (Spd x _)   = x

speed2 :: Speed a b -> b
speed2 (Spd _ y) = y

speedSet1 :: a -> Speed a b -> Speed a b
speedSet1 x (Spd _ y)   = Spd x y

speedSet2 :: b -> Speed a b -> Speed a b
speedSet2 y (Spd x _) = Spd x y

showSpdQ :: Spd2 -> [String]
showSpdQ = words . show
