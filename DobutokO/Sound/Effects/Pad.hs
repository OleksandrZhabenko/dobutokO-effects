-- |
-- Module      :  DobutokO.Sound.Effects.Pad
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"pad\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Pad where

import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.One

data PadSpec a = PS a a deriving Eq

instance Show (PadSpec TSpecification) where
  show (PS x y) = mconcat [show x,"@",show y," "]

type PadS = PadSpec TSpecification

padSpec1 :: PadSpec a -> a
padSpec1 (PS x _)   = x

padSpec2 :: PadSpec a -> a
padSpec2 (PS _ y)   = y

padSpecSet1 :: a -> PadSpec a -> PadSpec a
padSpecSet1 x (PS _ y) = PS x y

padSpecSet2 :: a -> PadSpec a -> PadSpec a
padSpecSet2 y (PS x _) = PS x y

showPSQ :: PadS -> [String]
showPSQ = words . show 

data Pad a b = PD1 [b] | PD2 a [b] | PD3 a a [b] deriving Eq

instance Show (Pad TSpecification PadS) where
  show (PD1 ys) 
    | null ys = ""
    | otherwise = mconcat ["pad ", mconcat . map show $ ys]
  show (PD2 x ys) 
    | null ys = mconcat ["pad ", show x]
    | otherwise = mconcat ["pad ", show x, " ", mconcat . map show $ ys]
  show (PD3 x z ys) 
    | null ys = mconcat ["pad ", show x, " ", show z, " "]
    | otherwise = mconcat ["pad ", show x, " ", mconcat . map show $ ys, show z, " "]

type Pad4 = Pad TSpecification PadS

padC :: Pad a b -> String
padC (PD1 _)   = "PD1"
padC (PD2 _ _)   = "PD2"
padC (PD3 _ _ _)    = "PD3"

pad1 :: Pad a b -> [a]
pad1 (PD1 _) = []
pad1 (PD2 x _) = [x]
pad1 (PD3 x y _) = [x,y]

pad2 :: Pad a b -> [b]
pad2 (PD1 ys) = ys
pad2 (PD2 _ ys) = ys
pad2 (PD3 _ _ ys) = ys

padSet1 :: One2 a -> Pad a b -> Pad a b
padSet1 (O21 x) (PD1 zs) = PD2 x zs
padSet1 (O22 x y) (PD1 zs) = PD3 x y zs
padSet1 (O21 x) (PD2 _ zs) = PD2 x zs
padSet1 (O22 x y) (PD2 _ zs) = PD3 x y zs
padSet1 (O21 x) (PD3 _ _ zs) = PD2 x zs
padSet1 (O22 x y) (PD3 _ _ zs) = PD3 x y zs

padSet2 :: [b] -> Pad a b -> Pad a b
padSet2 zs (PD1 _) = PD1 zs
padSet2 zs (PD2 x _) = PD2 x zs
padSet2 zs (PD3 x y _) = PD3 x y zs

showPaQ :: Pad4 -> [String]
showPaQ = words . show 
