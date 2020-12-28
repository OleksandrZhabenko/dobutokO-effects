-- |
-- Module      :  DobutokO.Sound.Effects.Segment
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"pitch\" and / or \"tempo\" SoX effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Segment where

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

data Segment a = E0 | S1 a | S2 a a | S3 a a a deriving Eq

instance Show (Segment Float) where
  show (E0) = ""
  show (S1 x) 
    | compare x 10.0 /= LT && compare x 120.0 /= GT = showFFloat Nothing x " "
    | otherwise = error "DobutokO.Sound.Effects.Segment.show: Not defined for the value. It must be in [10.0..120.0]. "
  show (S2 x y) 
    | compare x 10.0 /= LT && compare x 120.0 /= GT && compare y 0.0 /= LT && compare y 30.0 /= GT = mconcat [showFFloat Nothing x " ", showFFloat Nothing y " "]
    | otherwise = error "DobutokO.Sound.Effects.Segment.show: Not defined for the values. The first one must be in [10.0..120.0] and the second one -- in [0.0..30.0]. "
  show (S3 x y z) 
    | compare x 10.0 /= LT && compare x 120.0 /= GT && compare y 0.0 /= LT && compare y 30.0 /= GT && compare z 0.0 /= LT && compare z 30.0 /= GT = 
        mconcat [showFFloat Nothing x " ", showFFloat Nothing y " ", showFFloat Nothing z " "]
    | otherwise = 
       error "DobutokO.Sound.Effects.Segment.show: Not defined for the values. The first one must be in [10.0..120.0], the second and the third ones -- in [0.0..30.0] . "

type Segm = Segment Float

segmentC :: Segment a -> String
segmentC E0 = "E0"
segmentC (S1 _) = "S1"
segmentC (S2 _ _) = "S2"
segmentC (S3 _ _ _) = "S3"

segment1 :: Segment a -> Maybe a
segment1 (S1 x) = Just x
segment1 (S2 x _) = Just x
segment1 (S3 x _ _) = Just x
segment1 _ = Nothing

segment2 :: Segment a -> Maybe a
segment2 (S2 _ y) = Just y
segment2 (S3 _ y _) = Just y
segment2 _ = Nothing

segment3 :: Segment a -> Maybe a
segment3 (S3 _ _ z) = Just z
segment3 _ = Nothing

segmentSet1 :: a -> Segment a -> Segment a
segmentSet1 x (S2 _ y) = S2 x y
segmentSet1 x (S3 _ y z) = S3 x y z
segmentSet1 x _ = S1 x

segmentSet2 :: a -> a -> Segment a -> Segment a
segmentSet2 x y (S3 _ _ z) = S3 x y z
segmentSet2 x y _ = S2 x y

segmentSet3 :: a -> a -> a -> Segment a
segmentSet3 = S3

data Qdash = E | Q deriving Eq

instance Show Qdash where
  show E = ""
  show Q = "-q "

  
