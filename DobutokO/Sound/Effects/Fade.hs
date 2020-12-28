-- |
-- Module      :  DobutokO.Sound.Effects.Fade
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"fade\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Fade where

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

data FadeType = Q | HFt | TFt | L | P deriving Eq

instance Show FadeType where
  show Q = "q"
  show HFt = "h"
  show TFt = "t"
  show L = "l"
  show P = "p"

data Fade2 a b =  Fd a [b] deriving Eq

instance Show (Fade2 FadeType String) where
  show (Fd fdtype xss) 
   | null xss = []
   | otherwise = mconcat ["fade ", show fdtype, " ", mconcat . intersperse " " . take 3 $ xss]

fade1 :: Fade2 a b -> a
fade1 (Fd y _) = y

fade2 :: Fade2 a b -> [b]
fade2 (Fd _ xs) = take 3 xs

fadeSet1 :: a -> Fade2 a b -> Fade2 a b
fadeSet1 x (Fd _ ys) = Fd x ys

fadeSet2 :: [b] -> Fade2 a b -> Fade2 a b
fadeSet2 ys (Fd x _) = Fd x (take 3 ys)

type Fade = Fade2 FadeType String

fade2E :: Int -> Fade -> String
fade2E n (Fd _ xss) 
 | n == 1 = if null xss then " " else head xss
 | n == 2 = if null . drop 1 $ xss then " " else xss !! 1
 | n == 3 = if null . drop 2 $ xss then " " else xss !! 2
 | otherwise = error "DobutokO.Sound.Effects.Fade.fade2E: The first argument is out of possible range [1..3]. "

fadeSet2E :: Int -> String -> Fade -> Fade
fadeSet2E n x (Fd y xss) 
 | compare n 0 == GT && compare n 4 == LT && compare (length xss) n /= LT = Fd y (mconcat [take (n - 1) xss,[x],drop n xss])
 | otherwise = error "DobutokO.Sound.Effects.Fade.fadeSet2E: The first argument is out of possible defined ranges. "

showQFade :: Fade -> [String]
showQFade = words . show
   
