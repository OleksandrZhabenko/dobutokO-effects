-- |
-- Module      :  DobutokO.Sound.Effects.Stretch
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"stretch\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Stretch where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data StretchP a = SR a a a deriving Eq

-- | the first argument can be less than 1.0 but it is not recommended. The default value is 20.0.
instance Show (StretchP Float) where
  show (SR x y z) = mconcat [showFFloat Nothing (abs x) " lin ", showFFloat Nothing (toRange 1.0 . abs $ y) " ", showFFloat Nothing (toRange 0.5 . abs $ z) " "] 

type StretchPF = StretchP Float

stretch1 :: StretchP a -> a
stretch1 (SR x _ _) = x

stretch2 :: StretchP a -> a
stretch2 (SR _ y _) = y

stretch3 :: StretchP a -> a
stretch3 (SR _ _ z) = z

stretchSet1 :: a -> StretchP a -> StretchP a
stretchSet1 x (SR _ y z) = SR x y z

stretchSet2 :: a -> StretchP a -> StretchP a
stretchSet2 y (SR x _ z) = SR x y z

stretchSet3 :: a -> StretchP a -> StretchP a
stretchSet3 z (SR x y _) = SR x y z

data Stretch2 a b = SR21 a | SR22 a b deriving Eq

instance Show (Stretch2 Float StretchPF) where
  show (SR21 x) = mconcat ["stretch ", if compare (abs x) 0.001 == LT then "0.001 " else showFFloat Nothing (abs x) " "]
  show (SR22 x y) = mconcat ["stretch ", if compare (abs x) 0.001 == LT then "0.001 " else showFFloat Nothing (abs x) " ", show y]

type Stretch = Stretch2 Float StretchPF

stretch2C :: Stretch2 a b -> String
stretch2C (SR21 _) = "SR21"
stretch2C _ = "SR22"

stretch21 :: Stretch2 a b -> a
stretch21 (SR21 x) = x
stretch21 (SR22 x _) = x

stretch22 :: Stretch2 a b -> Maybe b
stretch22 (SR22 _ y) = Just y
stretch22 _ = Nothing

stretch2Set1 :: a -> Stretch2 a b -> Stretch2 a b
stretch2Set1 x (SR21 _) = SR21 x
stretch2Set1 x (SR22 _ y) = SR22 x y

stretch2Set2 :: b -> Stretch2 a b -> Stretch2 a b
stretch2Set2 y (SR21 x) = SR22 x y
stretch2Set2 y (SR22 x _) = SR22 x y

showSTRQ :: Stretch -> [String]
showSTRQ = words . show 
