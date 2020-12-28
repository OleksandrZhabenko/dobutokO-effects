-- |
-- Module      :  DobutokO.Sound.Effects.Flanger
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"flanger\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Flanger where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import DobutokO.Sound.One

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data ShapeInterp = S | T | L | Q deriving Eq

instance Show ShapeInterp where
  show S = "s "
  show T = "t "
  show L = "l "
  show Q = "q "

data Flanger a b = FL [a] b deriving Eq

defaultList :: [Float]
defaultList = [0.0, 2.0, 0.0, 71.0, 0.5, 25.0]

flElem1 :: Int -> Float -> Float
flElem1 n x 
  | n == 1 = if x == 0.0 then 0.0 else toRange 30.0 (abs x)
  | n == 2 = if x == 0.0 then 0.0 else toRange 10.0 (abs x)
  | n == 3 = if x == 0.0 then 0.0 else toRange 95.0 x
  | n == 4 = if x == 0.0 then 0.0 else toRange 100.0 (abs x)
  | n == 5 = if compare (toRange 10.0 . abs $ x) 0.1 == LT then 0.1 else toRange 10.0 (abs x)
  | n == 6 = if x == 0.0 then 0.0 else toRange 100.0 (abs x)
  | otherwise = error "DobutokO.Sound.Effects.Flanger.flElem1: the Int parameter must be in the range [1..6]. "

listFlanger1 :: [Float] -> [Float]
listFlanger1 xs 
  | compare (length xs) 6 == GT = map (\i -> flElem1 i (xs !! (i - 1))) [1..6]
  | otherwise = mconcat [map (\i -> flElem1 i (xs !! (i - 1))) [1..length xs], drop (length xs) defaultList]

listFlanger15 :: [Float] -> [Float]
listFlanger15 xs = take 5 . listFlanger1 $ xs

listFlanger16 :: [Float] -> Float
listFlanger16 xs = (listFlanger1 xs) !! 5

instance Show (Flanger Float (One2 ShapeInterp)) where
  show (FL xs (O21 T)) = mconcat ["flanger ", mconcat . map (\t -> showFFloat Nothing t " ") . listFlanger15 $ xs, show T, showFFloat Nothing (listFlanger16 xs) " "]
  show (FL xs (O21 y)) = mconcat ["flanger ", mconcat . map (\t -> showFFloat Nothing t " ") . listFlanger15 $ xs, show S, showFFloat Nothing (listFlanger16 xs) " ", 
     if y == S then "" else show y]
  show (FL xs (O22 L x)) = show (FL xs (O21 x))
  show (FL xs (O22 S x)) = show (FL xs (O21 x))
  show (FL xs (O22 x L)) = show (FL xs (O21 x))
  show (FL xs (O22 x S)) = show (FL xs (O21 x))
  show (FL xs ~(O22 x y)) 
    | x == y = show (FL xs (O21 x))
    | otherwise = mconcat ["flanger ", mconcat . map (\t -> showFFloat Nothing t " ") . listFlanger15 $ xs, show T, showFFloat Nothing (listFlanger16 xs) " ",
        show Q]

type Flanger2 = Flanger Float (One2 ShapeInterp)

flanger1 :: Flanger a b -> [a]
flanger1 (FL xs _) = xs

flanger2 :: Flanger a b -> b
flanger2 (FL _ y) = y

flanger1E :: Flanger2 -> [Float]
flanger1E (FL xs _) = listFlanger1 xs

flangerSet1 :: [a] -> Flanger a b -> Flanger a b
flangerSet1 xs (FL _ y) = FL xs y

flangerSet2 :: b -> Flanger a b -> Flanger a b
flangerSet2 y (FL xs _) = FL xs y

showFLQ :: Flanger2 -> [String]
showFLQ = words . show 
