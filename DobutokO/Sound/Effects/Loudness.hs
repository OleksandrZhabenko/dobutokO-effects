-- |
-- Module      :  DobutokO.Sound.Effects.Loudness
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"loudness\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Loudness where

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

data Loudness a = L | L1 a | L2 a a deriving Eq

instance Show (Loudness Float) where
  show (L1 x)
    | compare x (-50.0) /= LT && compare x 15.0 /= GT = mconcat ["loudness ", showFFloat Nothing x " "]
    | otherwise = ""
  show (L2 x y)
    | compare x (-50.0) /= LT && compare x 15.0 /= GT && compare y 50.0 /= LT && compare y 75.0 /= GT = mconcat ["loudness ", showFFloat Nothing x " ", 
       showFFloat Nothing y " "]
    | otherwise = ""
  show _ = "loudness "

type Ldness = Loudness Float

loudnessC :: Loudness a -> String
loudnessC L = "L"
loudnessC (L1 _) = "L1"
loudnessC _ = "L2"

loudness1 :: Loudness a -> Maybe a
loudness1 (L1 x) = Just x
loudness1 (L2 x _) = Just x
loudness1 _ = Nothing

loudness2 :: Loudness a -> Maybe a
loudness2 (L2 _ y) = Just y
loudness2 _ = Nothing

loudnessSet1 :: a -> Loudness a -> Loudness a
loudnessSet1 x (L2 _ y) = L2 x y
loudnessSet1 x _ = L1 x

loudnessSet2 :: a -> Loudness a -> Loudness a
loudnessSet2 y (L2 x _) = L2 x y
loudnessSet2 y (L1 x) = L2 x y
loudnessSet2 _ _ = L

showLdQ :: Ldness -> [String]
showLdQ = words . show 
