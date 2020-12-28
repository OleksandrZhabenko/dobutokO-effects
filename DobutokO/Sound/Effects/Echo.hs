-- |
-- Module      :  DobutokO.Sound.Effects.Echo
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"echo\" or \"echos\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Echo where

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

data EchoTail a = ET a a deriving Eq

instance Show (EchoTail Float) where
  show (ET delay decay) = mconcat [showFFloat Nothing (abs delay) " ", showFFloat Nothing (abs decay) " "]

type EchoTail1 = EchoTail Float

data Echo a b = E1 a a [b] deriving Eq

instance Show (Echo Float EchoTail1) where
  show (E1 gin gout ys) 
    | null ys = ""
    | otherwise = mconcat ["echo ", showFFloat Nothing (abs gin) " ", showFFloat Nothing (abs gout) " ", mconcat . intersperse " " . map show $ ys]

type Echo1 = Echo Float EchoTail1  

echoTail1 :: Int -> EchoTail a -> a
echoTail1 n (ET x0 x1) 
  | n == 1 = x0
  | n == 2 = x1
  | otherwise = error "DobutokO.Sound.Effects.Echo.echoTail1: Not defined parameter. "
  
echoTailSet1 :: Int -> a -> EchoTail a -> EchoTail a
echoTailSet1 n x (ET x0 x1) 
  | n == 1 = ET x x1 
  | n == 2 = ET x0 x 
  | otherwise = error "DobutokO.Sound.Effects.Echo.echoTailSet1: Not defined parameter. "

echo1 :: Int -> Echo a b -> a
echo1 n (E1 x0 x1 _) 
  | n == 1 = x0
  | n == 2 = x1
  | otherwise = error "DobutokO.Sound.Effects.Echo.echo1: Not defined parameter. "
  
echo2 :: Echo a b -> [b]
echo2 (E1 _ _ ys) = ys

echoSet1 :: Int -> a -> Echo a b -> Echo a b
echoSet1 n x (E1 x0 x1 y) 
  | n == 1 = E1 x x1 y
  | n == 2 = E1 x0 x y
  | otherwise = error "DobutokO.Sound.Effects.Echo.echoSet1: Not defined parameter. "

echoSet2 :: [b] -> Echo a b -> Echo a b
echoSet2 ys (E1 x0 x1 _) = E1 x0 x1 ys

showEchQ :: Echo1 -> [String]
showEchQ = words . show

data Echos a b = ES a a [b] deriving Eq

instance Show (Echos Float EchoTail1) where
  show (ES gin gout ys) 
    | null ys = ""
    | otherwise = mconcat ["echos ", showFFloat Nothing (abs gin) " ", showFFloat Nothing (abs gout) " ", mconcat . intersperse " " . map show $ ys]

type Echos1 = Echos Float EchoTail1  

echos1 :: Int -> Echos a b -> a
echos1 n (ES x0 x1 _) 
  | n == 1 = x0
  | n == 2 = x1
  | otherwise = error "DobutokO.Sound.Effects.Echo.echos1: Not defined parameter. "
  
echos2 :: Echos a b -> [b]
echos2 (ES _ _ ys) = ys

echosSet1 :: Int -> a -> Echos a b -> Echos a b
echosSet1 n x (ES x0 x1 y) 
  | n == 1 = ES x x1 y
  | n == 2 = ES x0 x y
  | otherwise = error "DobutokO.Sound.Effects.Echo.echosSet1: Not defined parameter. "

echosSet2 :: [b] -> Echos a b -> Echos a b
echosSet2 ys (ES x0 x1 _) = ES x0 x1 ys

showEchsQ :: Echos1 -> [String]
showEchsQ = words . show
