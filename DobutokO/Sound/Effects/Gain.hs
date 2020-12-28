-- |
-- Module      :  DobutokO.Sound.Effects.Gain
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"gain\" and \"norm\" effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Gain where

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

data FirstO = N1 | E | B | Bc | R deriving Eq

instance Show FirstO where
  show E = "-e "
  show B = "-B "
  show Bc = "-b "
  show R = "-r "
  show _ = ""

data SecondO = N2 | E0 deriving Eq

instance Show SecondO where
  show N2 = "-n "
  show _ = ""

data ThirdO = L | H | N3 deriving Eq

instance Show ThirdO where
  show L = "-l "
  show H = "-h "
  show _ = ""

data Gain1 a b c d = G a b c d deriving Eq

instance Show (Gain1 FirstO SecondO ThirdO Float) where
  show (G x y z t) = mconcat ["gain ", show x, show y, show z, showFFloat Nothing t " "]

type Gain = Gain1 FirstO SecondO ThirdO Float

gain1 :: Gain1 a b c d -> a
gain1 (G x _ _ _) = x

gain2 :: Gain1 a b c d -> b
gain2 (G _ y _ _) = y

gain3 :: Gain1 a b c d -> c
gain3 (G _ _ z _) = z

gain4 :: Gain1 a b c d -> d
gain4 (G _ _ _ t) = t

gainSet1 :: a -> Gain1 a b c d -> Gain1 a b c d
gainSet1 x (G _ y z t) = G x y z t

gainSet2 :: b -> Gain1 a b c d -> Gain1 a b c d
gainSet2 y (G x _ z t) = G x y z t

gainSet3 :: c -> Gain1 a b c d -> Gain1 a b c d
gainSet3 z (G x y _ t) = G x y z t

gainSet4 :: d -> Gain1 a b c d -> Gain1 a b c d
gainSet4 t (G x y z _) = G x y z t

showGQ :: Gain -> [String]
showGQ = words . show

data Norm = Norm | N4 deriving Eq

instance Show Norm where
  show Norm = "norm "
  show _ = ""
