-- |
-- Module      :  DobutokO.Sound.Effects.Misc
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying some of the SoX effects (first of all those ones without any passible parameters) 
-- and / or some of their combinations. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP #-}

module DobutokO.Sound.Effects.Misc where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import Data.List (intersperse)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Misc = D | E | OO | RE | RI | S deriving Eq

instance Show Misc where
  show D = "deemph "
  show E = "earwax "
  show OO = "oops "
  show RE = "reverse "
  show RI = "riaa "
  show S = "swap "

data MscS a = Msc [a] deriving Eq

instance (Show a) => Show (MscS a) where
  show (Msc ys) 
    | null ys = []
    | otherwise = mconcat [mconcat . intersperse " " . map show $ ys, " "]

type Mscs = MscS Misc

mscS1 :: MscS a -> [a]
mscS1 (Msc xs) = xs

mscSSet1 :: [a] -> MscS a
mscSSet1 = Msc

showMscQ :: Show a => MscS a -> [String]
showMscQ = words . show
