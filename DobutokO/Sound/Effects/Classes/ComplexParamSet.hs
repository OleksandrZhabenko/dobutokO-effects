-- |
-- Module      :  DobutokO.Sound.Effects.Classes.ComplexParamSet
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module DobutokO.Sound.Effects.Classes.ComplexParamSet where

import DobutokO.Sound.Effects.Segment
import DobutokO.Sound.Effects.Silence
import DobutokO.Sound.Effects.Sinc
import DobutokO.Sound.Effects.Timespec

class Complex2ParamSet a b c where
  set21c :: a -> b -> c

instance Complex2ParamSet a b (Duration a b) where
   set21c x y = durationSet x y 3

instance Complex2ParamSet Position Float FirstTSpec where
  set21c = seconds2FstTSpec

instance Complex2ParamSet Position Int FirstTSpec where
  set21c = samples2FstTSpec
  
instance Complex2ParamSet Position2 Float NextTSpec where
  set21c = seconds2NextTSpec

instance Complex2ParamSet Position2 Int NextTSpec where
  set21c = samples2NextTSpec
   
class Complex3ParamSet a b c d where
  set31c :: a -> b -> c -> d

instance Complex3ParamSet STSpecification1 Duration2 Threshold1 ATSpec where
  set31c = aboveTSpecSet1

instance Complex3ParamSet STSpecification2 Duration2 Threshold1 BTSpec where
  set31c = belowTSpecSet1
     
class BoolParamSet1 a b where
  set1B :: Bool -> a -> b

instance BoolParamSet1 a (SincAB a) where
  set1B = sincABSet1

instance BoolParamSet1 a (SincTN a) where
  set1B = sincTNSet1

class ComplexParamSet2 a b where
  set2c :: a -> a -> b -> b

instance ComplexParamSet2 a (Segment a) where
  set2c = segmentSet2

class ComplexParamSet3 a b where
  set3c :: a -> a -> a -> b

instance ComplexParamSet3 a (Segment a) where
  set3c = segmentSet3

