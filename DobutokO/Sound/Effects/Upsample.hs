-- |
-- Module      :  DobutokO.Sound.Effects.Upsample
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"upsample\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Upsample where

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

data Upsample a = U | US1 a deriving Eq

instance Show (Upsample Int) where
  show (US1 x) = mconcat ["upsample ", if x == 0 then "" else show (abs x) ++ " "]
  show _ = "upsample "
  
type USample = Upsample Int 

upsampleC :: Upsample a -> String
upsampleC U = "U"
upsampleC _ = "US1"

upSample1 :: Upsample a -> Maybe a
upSample1 (US1 x) = Just x
upSample1 _ = Nothing

upSampleSet1 :: a -> Upsample a
upSampleSet1 = US1

showUSQ :: USample -> [String]
showUSQ = words . show 
