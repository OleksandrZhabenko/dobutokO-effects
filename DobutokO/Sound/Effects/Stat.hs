-- |
-- Module      :  DobutokO.Sound.Effects.Stat
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"stat\" effect.
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Stat where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.Effects.Misc (MscS(..))

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data StatP a = S a | RMS | V | Freq | D deriving Eq

instance Show (StatP Float) where
  show (S x) = mconcat ["-s ", showFFloat Nothing x " "]
  show RMS = "-rms "
  show V = "-v "
  show D = "-d "
  show _ = "-freq "

type StatP1 = StatP Float

statPC :: StatP a -> String
statPC (S _)   = "S"
statPC RMS = "RMS"
statPC V = "V"
statPC D = "D"
statPC _ = "Freq"

statP1 :: StatP a -> Maybe a
statP1 (S x) = Just x
statP1 _ = Nothing

statPSet1 :: a -> StatP a
statPSet1 = S

data Stat1 a = ST (MscS a) deriving Eq

instance Show (Stat1 StatP1) where
  show (ST (Msc xs)) = mconcat ["stat ", show (Msc xs)]

type Stat = Stat1 StatP1

stat11 :: Stat1 a -> [a]
stat11 (ST (Msc xs)) = xs

stat1Set1 :: [a] -> Stat1 a
stat1Set1 xs = ST (Msc xs)

showSTQ :: Stat -> [String]
showSTQ = words . show 
