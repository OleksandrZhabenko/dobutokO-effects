-- |
-- Module      :  DobutokO.Sound.Effects.Contrast
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"contrast\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Contrast where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import DobutokO.Sound.ToRange
import Numeric (showFFloat)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Contrast a = E | Ct a deriving Eq

instance Show (Contrast Float) where
  show E = "contrast 75"
  show (Ct x) = mconcat ["contrast ", showFFloat Nothing (toRange 100.0 . abs $ x) " "]

type Cntrst = Contrast Float

contrastC :: Contrast a -> String
contrastC E = "E"
contrastC _ = "Ct"

contrast1 :: Contrast a -> Maybe a
contrast1 (Ct x) = Just x
contrast1 _ = Nothing

contrastE1 :: Cntrst -> Float
contrastE1 (Ct x) = x
contrastE1 E = 75.0

contrastSet1 :: a -> Contrast a -> Contrast a
contrastSet1 x _ = Ct x

showCtQ :: Cntrst -> [String]
showCtQ = words . show
