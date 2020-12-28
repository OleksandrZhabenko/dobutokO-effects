-- |
-- Module      :  DobutokO.Sound.Effects.Specs
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects with the needed specifications. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Specs where

import Numeric (showFFloat)

data Frequency a = FHz a | FkHz a deriving Eq

instance Show (Frequency Float) where
  show (FHz x) = showFFloat Nothing x " "
  show (FkHz x) = showFFloat Nothing x "k "
  
type Freq1 = Frequency Float  

frequency1 :: Freq1 -> Float
frequency1 (FHz x) = x
frequency1 (FkHz x) = 1000.0 * x

frequencySet1 ::  Float -> Freq1 -> Freq1
frequencySet1 x (FHz _) = FHz x
frequencySet1 x (FkHz _) = FkHz x

data Width a = H a | K a | O a | Q a deriving Eq

instance Show (Width Float) where 
  show (H x) = showFFloat Nothing x "h"
  show (K x) = showFFloat Nothing x "k"
  show (O x) = showFFloat Nothing x "o"
  show (Q x) = showFFloat Nothing x "q"

type Width1 = Width Float

width1 :: Width a -> a
width1 (H x) = x
width1 (K x) = x
width1 (O x) = x
width1 (Q x) = x

widthSet1 :: a -> Width a -> Width a
widthSet1 x (H _) = H x
widthSet1 x (K _) = K x
widthSet1 x (O _) = O x
widthSet1 x (Q _) = Q x
