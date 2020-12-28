-- |
-- Module      :  DobutokO.Sound.Effects.Biquad
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"biquad\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Biquad where

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

data Coeffs a = BQ3 a a a deriving Eq

instance Show (Coeffs Float) where
  show (BQ3 x0 x1 x2) = mconcat [showFFloat Nothing x0 " ", showFFloat Nothing x1 " ", showFFloat Nothing x2 " "]

type BiQuad3 = Coeffs Float

coeffs1 :: Int -> Coeffs a -> Maybe a
coeffs1 n (BQ3 x0 x1 x2) 
  | compare n 0 == GT && compare n 4 == LT = 
     case n of 
      1 -> Just x0
      2 -> Just x1
      _ -> Just x2
  | otherwise = Nothing

coeffsSet1 :: a -> Coeffs a -> Coeffs a
coeffsSet1 x0 (BQ3 _ x1 x2) = BQ3 x0 x1 x2

coeffsSet2 :: a -> Coeffs a -> Coeffs a
coeffsSet2 x1 (BQ3 x0 _ x2) = BQ3 x0 x1 x2

coeffsSet3 :: a -> Coeffs a -> Coeffs a
coeffsSet3 x2 (BQ3 x0 x1 _) = BQ3 x0 x1 x2

data Biquad a = BQ (Coeffs a) (Coeffs a) deriving Eq

instance Show (Biquad Float) where
  show (BQ x y) = mconcat ["biquad ",show x, show y]

type BiQuad6 = Biquad Float

biquad1 :: Biquad a -> Coeffs a
biquad1 (BQ x _) = x

biquad2 :: Biquad a -> Coeffs a
biquad2 (BQ _ y) = y

biquadSet1 :: Coeffs a -> Biquad a -> Biquad a
biquadSet1 x (BQ _ y) = BQ x y

biquadSet2 :: Coeffs a -> Biquad a -> Biquad a
biquadSet2 y (BQ x _) = BQ x y

showBQ6Q :: BiQuad6 -> [String]
showBQ6Q = words . show
