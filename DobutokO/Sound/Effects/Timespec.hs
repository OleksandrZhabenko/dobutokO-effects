-- |
-- Module      :  DobutokO.Sound.Effects.Timespec
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects with the needed time specifications. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Timespec where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.One

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Position = P | M | E deriving Eq

instance Show Position where
  show P = "+"
  show M = "-"
  show E = "="

data TSpec a b c = Ts a c | Tm a b c | Th a b b c | S a b deriving Eq

instance Show (TSpec Position Int Float) where 
  show (Ts x y) = mconcat [show x,showFFloat Nothing (abs y) "t"]
  show (Tm x y z) = mconcat [show x,show (abs y),":",showFFloat Nothing (abs z) "t"]
  show (Th x y1 y2 z) = mconcat [show x,show (abs y1),":",show (abs y2),":",showFFloat Nothing (abs z) "t"]
  show (S x y) = mconcat [show x,show (abs y),"s"]

type FirstTSpec = TSpec Position Int Float

isTime :: TSpec a b c -> Bool
isTime (S _ _) = False
isTime _ = True

isSamples :: TSpec a b c -> Bool
isSamples (S _ _) = True
isSamples _ = False

tSpecC :: FirstTSpec -> String
tSpecC (Ts _ _) = "Ts"
tSpecC (Tm _ _ _) = "Tm"
tSpecC (Th _ _ _ _) = "Th"
tSpecC (S _ _) = "S"

tSpecPos :: FirstTSpec -> Position
tSpecPos (Ts x _) = x
tSpecPos (Tm x _ _) = x
tSpecPos (Th x _ _ _) = x
tSpecPos (S x _) = x

seconds :: FirstTSpec -> Maybe Float
seconds (Ts _ x) = Just (abs x)
seconds (Tm _ x y) = Just (abs y + fromIntegral (60 * abs x))
seconds (Th _ x y z) = Just (abs z + fromIntegral (3600 * abs x + 60 * abs y))
seconds _ = Nothing

minutes :: FirstTSpec -> Maybe Int
minutes (Ts _ x) = Just (truncate $ abs x / 60.0)
minutes (Tm _ x y) = Just (abs x + truncate (abs y / 60.0))
minutes (Th _ x y z) = Just (abs y + truncate (abs z / 60.0) + 60 * abs x)
minutes _ = Nothing

hours :: FirstTSpec -> Maybe Int
hours (Ts _ x) = Just (truncate $ abs x / 3600.0)
hours (Tm _ x y) = Just (truncate (fromIntegral (abs x) / 60.0 + abs y / 3600.0))
hours (Th _ x y z) = Just (abs x + truncate (abs z / 3600.0 + fromIntegral (abs y) / 60.0))
hours _ = Nothing

samples :: FirstTSpec -> Maybe Int
samples (S _ x) = Just x
samples _ = Nothing

seconds2FstTSpec :: Position -> Float -> FirstTSpec
seconds2FstTSpec x y = Ts x y

samples2FstTSpec :: Position -> Int -> FirstTSpec
samples2FstTSpec x y = S x (abs y)

data Position2 = P2 | M2 deriving Eq

instance Show Position2 where
  show P2 = "+"
  show M2 = "-"

instance Show (TSpec Position2 Int Float) where
  show (Ts u x) = mconcat [show u,showFFloat Nothing (abs x) "t"]
  show (Tm u x y) = mconcat [show u,show (abs x),":",showFFloat Nothing (abs y) "t"]
  show (Th u x0 x y) = mconcat [show u,show (abs x0),":",show (abs x),":",showFFloat Nothing (abs y) "t"]
  show (S u x) = mconcat [show u,show (abs x),"s"]

type NextTSpec = TSpec Position2 Int Float  

tSpecC2 :: NextTSpec -> String
tSpecC2 (Ts _ _) = "Ts"
tSpecC2 (Tm _ _ _) = "Tm"
tSpecC2 (Th _ _ _ _) = "Th"
tSpecC2 (S _ _) = "S"

tSpecPos2 :: NextTSpec -> Position2
tSpecPos2 (Ts x _) = x
tSpecPos2 (Tm x _ _) = x
tSpecPos2 (Th x _ _ _) = x
tSpecPos2 (S x _) = x

seconds2 :: NextTSpec -> Maybe Float
seconds2 (Ts _ x) = Just (abs x)
seconds2 (Tm _ x y) = Just (abs y + fromIntegral (60 * abs x))
seconds2 (Th _ x y z) = Just (abs z + fromIntegral (3600 * abs x + 60 * abs y))
seconds2 _ = Nothing

minutes2 :: NextTSpec -> Maybe Int
minutes2 (Ts _ x) = Just (truncate $ abs x / 60.0)
minutes2 (Tm _ x y) = Just (abs x + truncate (abs y / 60.0))
minutes2 (Th _ x y z) = Just (abs y + truncate (abs z / 60.0) + 60 * abs x)
minutes2 _ = Nothing

hours2 :: NextTSpec -> Maybe Int
hours2 (Ts _ x) = Just (truncate $ abs x / 3600.0)
hours2 (Tm _ x y) = Just (truncate (fromIntegral (abs x) / 60.0 + abs y / 3600.0))
hours2 (Th _ x y z) = Just (abs x + truncate (abs z / 3600.0 + fromIntegral (abs y) / 60.0))
hours2 _ = Nothing

samples2 :: NextTSpec -> Maybe Int
samples2 (S _ x) = Just x
samples2 _ = Nothing

seconds2NextTSpec :: Position2 -> Float -> NextTSpec
seconds2NextTSpec x y = Ts x y

samples2NextTSpec :: Position2 -> Int -> NextTSpec
samples2NextTSpec x y = S x (abs y)

data TimeSpec a b = TS1 a | TS2 a [b] deriving Eq

isFirstTS :: TimeSpec a b -> Bool
isFirstTS (TS1 _) = True
isFirstTS _ = False

isExtTS :: TimeSpec a b -> Bool
isExtTS (TS2 _ _) = True
isExtTS _ = False

timeSpecC :: TimeSpec a b -> String
timeSpecC (TS1 _) = "TS1"
timeSpecC (TS2 _ _) = "TS2"

timeSpec1 :: TimeSpec a b -> a
timeSpec1 (TS1 x) = x
timeSpec1 (TS2 x _) = x

timeSpec2 :: TimeSpec a b -> Maybe [b]
timeSpec2 (TS2 _ ys) = Just ys
timeSpec2 _ = Nothing

timeSpecSet1 :: a -> TimeSpec a b -> TimeSpec a b
timeSpecSet1 x (TS1 _) = TS1 x
timeSpecSet1 x (TS2 _ ys) = TS2 x ys

timeSpecSet2 :: [b] -> TimeSpec a b -> TimeSpec a b
timeSpecSet2 xs (TS1 y) = TS2 y xs
timeSpecSet2 xs (TS2 y _) = TS2 y xs

instance Show (TimeSpec FirstTSpec NextTSpec) where
  show (TS1 x) = show x
  show (TS2 x ys) = mconcat [show x,mconcat . map show $ ys]

type TSpecification = TimeSpec FirstTSpec NextTSpec

instance Show (One3 TSpecification) where
  show (O31 x) = mconcat [show x, " "]
  show (O32 x y) = mconcat [show x, ",", show y, " "]
  show (O33 x y z) = mconcat [show x, ",", show y, ",", show z, " "]
  
