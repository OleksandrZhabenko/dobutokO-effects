-- |
-- Module      :  DobutokO.Sound.Effects.FIR
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"fir\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.FIR where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import Data.List (intersperse)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Fir a b = CF a | Cs [b] | EF deriving Eq

instance Show (Fir FilePath Float) where
  show (CF file) = mconcat ["fir ", file, " "]
  show (Cs xs) = mconcat ["fir ", mconcat . intersperse " " . map (\x -> showFFloat Nothing x " ") $ xs]
  show _ = "" -- the shell command will expect input from stdin. If it is not prepared, planned and available, do not use at all.

type FIR = Fir FilePath Float

firC :: Fir a b -> String
firC (CF _) = "CF"
firC (Cs _) = "Cs"
firC _ ="EF"

fir1 :: Fir a b -> Maybe a
fir1 (CF x) = Just x
fir1 _ = Nothing

fir2 :: Fir a b -> Maybe [b]
fir2 (Cs xs) = Just xs
fir2 _ = Nothing

firSet1 :: a -> Fir a b
firSet1 = CF

firSet2 :: [b] -> Fir a b
firSet2 = Cs

showFIRQ :: FIR -> [String]
showFIRQ = words . show 
