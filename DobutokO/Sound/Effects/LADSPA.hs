-- |
-- Module      :  DobutokO.Sound.Effects.LADSPA
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"ladspa\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.LADSPA where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import DobutokO.Sound.One

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data LADSPAS1 = L | R | N0 deriving Eq

instance Show LADSPAS1 where
  show L = "-l "
  show R = "-r "
  show _ = ""

data Ladspa1 a b c = LPA2 a b | LPA3 a b b | LP3 a b c | LP4 a b b c deriving Eq

instance (Show c) => Show (Ladspa1 LADSPAS1 String c) where
  show (LPA2 x ys) = mconcat ["ladspa ", show x, ys, " "]
  show (LPA3 x ys zs) = mconcat ["ladspa ", show x, ys, " ", zs, " "]
  show (LP3 x ys z) = mconcat ["ladspa ", show x, ys, " ", show z, " "]
  show (LP4 x ys zs t) = mconcat ["ladspa ", show x, ys, " ", zs, " ", show t, " "]

type Ladspa c = Ladspa1 LADSPAS1 String c

ladspaC :: Ladspa1 a b c -> String
ladspaC (LPA2 _ _) = "LPA2"
ladspaC (LPA3 _ _ _) = "LPA3"
ladspaC (LP3 _ _ _) = "LP3"
ladspaC (LP4 _ _ _ _) = "LP4"

ladspa1 :: Ladspa1 a b c -> a
ladspa1 (LPA2 x _) = x
ladspa1 (LPA3 x _ _) = x
ladspa1 (LP3 x _ _) = x
ladspa1 (LP4 x _ _ _) = x

ladspa2 :: Ladspa1 a b c -> One2 b
ladspa2 (LPA2 _ y) = O21 y
ladspa2 (LPA3 _ y z) = O22 y z
ladspa2 (LP3 _ y _) = O21 y
ladspa2 (LP4 _ y1 y2 _) = O22 y1 y2

ladspa3 :: Ladspa1 a b c -> Maybe c
ladspa3 (LP3 _ _ z) = Just z
ladspa3 (LP4 _ _ _ z) = Just z
ladspa3 _ = Nothing

ladspaSet1 :: a -> Ladspa1 a b c -> Ladspa1 a b c
ladspaSet1 x (LPA2 _ y) = LPA2 x y
ladspaSet1 x (LPA3 _ y1 y2) = LPA3 x y1 y2
ladspaSet1 x (LP3 _ y z) = LP3 x y z
ladspaSet1 x (LP4 _ y1 y2 z) = LP4 x y1 y2 z

ladspaSet2 :: One2 b -> Ladspa1 a b c -> Ladspa1 a b c
ladspaSet2 (O21 y) (LPA2 x _) = LPA2 x y
ladspaSet2 (O21 y) (LPA3 x _ y2) = LPA3 x y y2
ladspaSet2 (O21 y) (LP3 x _ z) = LP3 x y z
ladspaSet2 (O21 y) (LP4 x _ y2 z) = LP4 x y y2 z
ladspaSet2 (O22 y1 y2) (LPA2 x _) = LPA3 x y1 y2
ladspaSet2 (O22 y1 y2) (LPA3 x _ _) = LPA3 x y1 y2
ladspaSet2 (O22 y1 y2) (LP3 x _ z) = LP4 x y1 y2 z
ladspaSet2 (O22 y1 y2) (LP4 x _ _ z) = LP4 x y1 y2 z

ladspaSet3 :: (Show c) => c -> Ladspa1 a b c -> Ladspa1 a b c
ladspaSet3 z (LPA2 x y) = LP3 x y z
ladspaSet3 z (LPA3 x y1 y2) = LP4 x y1 y2 z
ladspaSet3 z (LP3 x y _) = LP3 x y z
ladspaSet3 z (LP4 x y1 y2 _) = LP4 x y1 y2 z

showLPQ :: (Show c) => Ladspa c -> [String]
showLPQ = words . show 
