-- |
-- Module      :  DobutokO.Sound.Effects.Tempo
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"tempo\" SoX effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Tempo where


import Numeric (showFFloat)
import DobutokO.Sound.Effects.Segment

data MSL = E | M | S | L deriving Eq

instance Show MSL where
  show M = "-m "
  show S = "-s "
  show L = "-l "
  show _ = ""

data Tempo a b c d = Tm3 a b c | Tm4 a b c d deriving Eq

instance Show (Tempo Qdash MSL Float Segm) where
  show (Tm3 x y z) = mconcat ["tempo ", show x,show y, showFFloat Nothing z " "]
  show (Tm4 x y z t) = mconcat ["tempo ", show x, show y, showFFloat Nothing z " ", show t]

type Tmp = Tempo Qdash MSL Float Segm

tempoC :: Tempo a b c d -> String
tempoC (Tm3 _ _ _) = "Tm3"
tempoC (Tm4 _ _ _ _) = "Tm4"

tempo1 :: Tempo a b c d -> a
tempo1 (Tm3 x _ _) = x
tempo1 (Tm4 x _ _ _) = x

tempo2 :: Tempo a b c d -> b
tempo2 (Tm3 _ y _) = y
tempo2 (Tm4 _ y _ _) = y

tempo3 :: Tempo a b c d -> c
tempo3 (Tm4 _ _ z _) = z
tempo3 (Tm3 _ _ z) = z

tempo4 :: Tempo a b c d -> Maybe d
tempo4 (Tm4 _ _ _ t) = Just t
tempo4 _ = Nothing

tempoSet1 :: a -> Tempo a b c d -> Tempo a b c d
tempoSet1 x (Tm3 _ y z) = Tm3 x y z
tempoSet1 x (Tm4 _ y z t) = Tm4 x y z t

tempoSet2 :: b -> Tempo a b c d -> Tempo a b c d
tempoSet2 y (Tm3 x _ z) = Tm3 x y z
tempoSet2 y (Tm4 x _ z t) = Tm4 x y z t

tempoSet3 :: c -> Tempo a b c d -> Tempo a b c d
tempoSet3 z (Tm4 x y _ t) = Tm4 x y z t
tempoSet3 z (Tm3 x y _) = Tm3 x y z

tempoSet4 :: d -> Tempo a b c d -> Tempo a b c d
tempoSet4 t (Tm3 x y z) = Tm4 x y z t
tempoSet4 t (Tm4 x y z _) = Tm4 x y z t

showTmpQ :: Tmp -> [String]
showTmpQ = words . show
