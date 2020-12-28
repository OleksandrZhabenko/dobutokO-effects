-- |
-- Module      :  DobutokO.Sound.Effects.Hilbert
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"hilbert\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Hilbert where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Hilbert a = H | HI a deriving Eq

instance Show (Hilbert Int) where
  show (HI n) 
    | compare n 2 == GT && compare n 32768 == LT = mconcat ["hilbert -n ",show n]
    | otherwise = ""
  show _ = "hilbert "

type Hlbrt = Hilbert Int

hilbertC :: Hilbert a -> String
hilbertC H = "H"
hilbertC _ = "HI"

hilbert1 :: Hilbert a -> Maybe a
hilbert1 (HI x) = Just x
hilbert1 _ = Nothing

hilbertSet1 :: a -> Hilbert a
hilbertSet1 = HI

showHIQ :: Hlbrt -> [String]
showHIQ = words . show 
