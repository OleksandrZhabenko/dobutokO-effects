-- |
-- Module      :  DobutokO.Sound.Effects.Stats
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"stats\" effect.
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Stats where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif

import Numeric (showFFloat)
import DobutokO.Sound.Effects.Misc (MscS(..),mscS1)
import DobutokO.Sound.ToRange

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data StatsP a = E | B a | X a | S a deriving Eq

instance Show (StatsP Float) where
  show (B x) = mconcat ["-b ", showFFloat Nothing (if compare (abs x) 2.0 == LT then 2.0 else (toRange 32.0 . abs $ x)) " "]
  show (X x) = mconcat ["-x ", showFFloat Nothing (if compare (abs x) 2.0 == LT then 2.0 else (toRange 32.0 . abs $ x)) " "]
  show (S x) = mconcat ["-s ", showFFloat Nothing (toRange 99.0 x) " "]
  show _ = ""

type StatsPF = StatsP Float

statsPC :: StatsP a -> String
statsPC (B _) = "B"
statsPC (X _) = "X"
statsPC (S _) = "S"
statsPC _ = "E"

statsP1 :: StatsP a -> Maybe a
statsP1 (B x) = Just x
statsP1 (X x) = Just x
statsP1 (S x) = Just x
statsP1 _ = Nothing

statsPSet1 :: a -> StatsP a -> StatsP a
statsPSet1 x (B _) = B x
statsPSet1 x (X _) = X x
statsPSet1 x (S _) = S x
statsPSet1 _ _ = E

data Window1 a = E0 | W a deriving Eq

instance Show (Window1 Float) where
  show (W x) = mconcat ["-w ", showFFloat Nothing (if compare (abs x) 0.01 == LT then 0.01 else (toRange 10.0 . abs $ x)) " "]
  show _ = ""

type Window = Window1 Float

window1C :: Window1 a -> String
window1C E0 = "E0"
window1C _ = "W"

window11 :: Window1 a -> Maybe a
window11 (W x) = Just x
window11 _ = Nothing

window1Set1 :: a -> Window1 a
window1Set1 = W

data Stats2 a b = STT (MscS a) (MscS b) deriving Eq

instance Show (Stats2 StatsPF Window) where
  show (STT x y) = mconcat ["stats ", show x, show y]

type Stats = Stats2 StatsPF Window

stats21 :: Stats2 a b -> [a]
stats21 (STT x _) = mscS1 x

stats22 :: Stats2 a b -> [b]
stats22 (STT _ y) = mscS1 y

stats2Set1 :: [a] -> Stats2 a b -> Stats2 a b
stats2Set1 xs (STT _ y) = STT (Msc xs) y

stats2Set2 :: [b] -> Stats2 a b -> Stats2 a b
stats2Set2 ys (STT x _) = STT x (Msc ys)

showSTTQ :: Stats -> [String]
showSTTQ = words . show 
