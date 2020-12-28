-- |
-- Module      :  DobutokO.Sound.Effects.Downsample
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"downsample\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Downsample where

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

data Downsample a = D | DS1 a deriving Eq

instance Show (Downsample Int) where
  show (DS1 x) = mconcat ["downsample ", if compare (abs x) 2 == LT then "2 " else show (abs x) ++ " "]
  show _ = "downsample 2 "
  
type DSample = Downsample Int 

downsampleC :: Downsample a -> String
downsampleC D = "D"
downsampleC _ = "DS1"

downSample1 :: Downsample a -> Maybe a
downSample1 (DS1 x) = Just x
downSample1 _ = Nothing

downSampleE1 :: DSample -> Int
downSampleE1 (DS1 x) = x
downSampleE1 _ = 2

downSampleSet1 :: a -> Downsample a
downSampleSet1 = DS1

showDSQ :: DSample -> [String]
showDSQ = words . show 
