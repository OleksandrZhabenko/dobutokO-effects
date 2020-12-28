-- |
-- Module      :  DobutokO.Sound.Effects.Modulation2
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying some of the SoX effects. 
-- 

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Effects.Modulation2 where


data Modulation = S | T deriving Eq

instance Show Modulation where
  show S = "-s "
  show T = "-t "
