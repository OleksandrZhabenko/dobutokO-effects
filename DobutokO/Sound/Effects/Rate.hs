-- |
-- Module      :  DobutokO.Sound.Effects.Rate
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"rate\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Rate where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import DobutokO.Sound.Effects.Specs (Freq1)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data RateTL = Q | L deriving Eq

instance Show RateTL where
  show Q = "-q "
  show _ = "-l "

data RateTH = M | H | V deriving Eq

instance Show RateTH where
  show M = "-m "
  show H = "-h "
  show _ = "-v "

data ROpt1 = N1 | M1 | I1 | L1 deriving Eq

instance Show ROpt1 where
  show M1 = "-M "
  show I1 = "-I "
  show L1 = "-L "
  show _ = ""

data ROpt2 = N2 | S2 deriving Eq

instance Show ROpt2 where
  show S2 = "-s "
  show _ = ""

data ROpt3 = N3 | A3 deriving Eq

instance Show ROpt3 where
  show A3 = "-a "
  show _ = ""

data Ropt4 a = N4 | B a deriving Eq

instance Show (Ropt4 Float) where
  show (B x) = mconcat ["-b ", if compare (toRange 99.7 (abs x)) 74.0 == LT then "74.0" else showFFloat Nothing (toRange 99.7 (abs x)) " "]
  show _ = ""

type ROpt4 = Ropt4 Float

rOpt4C :: Ropt4 a -> String
rOpt4C (B _) = "B"
rOpt4C _ = "N4"

rOpt41 :: Ropt4 a -> Maybe a
rOpt41 (B x) = Just x
rOpt41 _ = Nothing

rOpt4Set1 :: a -> Ropt4 a
rOpt4Set1 = B

data Ropt5 a = N5 | P a deriving Eq

instance Show (Ropt5 Float) where
  show (P x) = mconcat ["-p ", showFFloat Nothing (toRange 100.0 (abs x)) " "]
  show _ = ""

type ROpt5 = Ropt5 Float

rOpt5C :: Ropt5 a -> String
rOpt5C (P _) = "P"
rOpt5C _ = "N5"

rOpt51 :: Ropt5 a -> Maybe a
rOpt51 (P x) = Just x
rOpt51 _ = Nothing

rOpt5Set1 :: a -> Ropt5 a
rOpt5Set1 = P

data RateL a b = RL a b deriving Eq

instance Show (RateL RateTL Freq1) where
  show (RL x y) = mconcat ["rate ", show x, show y]

type RateLow = RateL RateTL Freq1

rateL1 :: RateL a b -> a
rateL1 (RL x _) = x

rateL2 :: RateL a b -> b
rateL2 (RL _ y) = y

rateLSet1 :: a -> RateL a b -> RateL a b
rateLSet1 x (RL _ y) = RL x y

rateLSet2 :: b -> RateL a b -> RateL a b
rateLSet2 y (RL x _) = RL x y

showRLQ :: RateLow -> [String]
showRLQ = words . show 

data RateH a b1 b2 b3 b4 b5 c = RH a b1 b2 b3 b4 b5 c deriving Eq

instance Show (RateH RateTH ROpt1 ROpt2 ROpt3 ROpt4 ROpt5 Freq1) where
  show (RH x y1 y2 y3 y4 y5 z) = mconcat ["rate ", show x, show y1, show y2, show y3, show y4, show y5, show z]

type RateHigh = RateH RateTH ROpt1 ROpt2 ROpt3 ROpt4 ROpt5 Freq1

rateH1 :: RateH a b1 b2 b3 b4 b5 c -> a
rateH1 (RH x _ _ _ _ _ _) = x

rateH21 :: RateH a b1 b2 b3 b4 b5 c -> b1
rateH21 (RH _ y1 _ _ _ _ _) = y1

rateH22 :: RateH a b1 b2 b3 b4 b5 c -> b2
rateH22 (RH _ _ y2 _ _ _ _) = y2

rateH23 :: RateH a b1 b2 b3 b4 b5 c -> b3
rateH23 (RH _ _ _ y3 _ _ _) = y3

rateH24 :: RateH a b1 b2 b3 b4 b5 c -> b4
rateH24 (RH _ _ _ _ y4 _ _) = y4

rateH25 :: RateH a b1 b2 b3 b4 b5 c -> b5
rateH25 (RH _ _ _ _ _ y5 _) = y5

rateH3 :: RateH a b1 b2 b3 b4 b5 c -> c
rateH3 (RH _ _ _ _ _ _ z) = z

rateHSet1 :: a -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet1 x (RH _ y1 y2 y3 y4 y5 z) = RH x y1 y2 y3 y4 y5 z

rateHSet21 :: b1 -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet21 y1 (RH x _ y2 y3 y4 y5 z) = RH x y1 y2 y3 y4 y5 z

rateHSet22 :: b2 -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet22 y2 (RH x y1 _ y3 y4 y5 z) = RH x y1 y2 y3 y4 y5 z

rateHSet23 :: b3 -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet23 y3 (RH x y1 y2 _ y4 y5 z) = RH x y1 y2 y3 y4 y5 z

rateHSet24 :: b4 -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet24 y4 (RH x y1 y2 y3 _ y5 z) = RH x y1 y2 y3 y4 y5 z

rateHSet25 :: b5 -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet25 y5 (RH x y1 y2 y3 y4 _ z) = RH x y1 y2 y3 y4 y5 z

rateHSet3 :: c -> RateH a b1 b2 b3 b4 b5 c -> RateH a b1 b2 b3 b4 b5 c
rateHSet3 z (RH x y1 y2 y3 y4 y5 _) = RH x y1 y2 y3 y4 y5 z

showRHQ :: RateHigh -> [String]
showRHQ = words . show 

data Rate2 a b = LR a | HR b deriving Eq

instance Show (Rate2 RateLow RateHigh) where 
  show (LR x) = show x
  show (HR x) = show x

type Rate = Rate2 RateLow RateHigh

rate2C :: Rate2 a b -> String
rate2C (LR _) = "LR"
rate2C _ = "HR"

rate21 :: Rate2 a b -> Maybe a
rate21 (LR x) = Just x
rate21 _ = Nothing

rate22 :: Rate2 a b -> Maybe b
rate22 (HR y) = Just y
rate22 _ = Nothing

rate2Set1 :: RateLow -> Rate
rate2Set1 = LR

rate2Set2 :: RateHigh -> Rate
rate2Set2 = HR

showRQ :: Rate -> [String]
showRQ = words . show 
