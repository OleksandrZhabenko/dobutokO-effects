-- |
-- Module      :  DobutokO.Sound.Effects.Repeat
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"repeat\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Repeat where

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

data Count a = I | O1 | Ct a deriving Eq

instance Show (Count Int) where
  show I = "- "
  show O1 = ""
  show (Ct n) 
   | n == 0 = "- "
   | otherwise = show n ++ " "

type CountR = Count Int  

data Repeat a = Rpt a deriving Eq

instance Show (Repeat CountR) where
  show (Rpt x) = mconcat ["repeat ",show x]

repeat1 :: Repeat a -> a
repeat1 (Rpt x) = x

repeatSet1 :: a -> Repeat a
repeatSet1 = Rpt

type Repeat1 = Repeat CountR

showRptQ :: Repeat1 -> [String]
showRptQ = words . show
