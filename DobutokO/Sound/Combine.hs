-- |
-- Module      :  DobutokO.Sound.Combine
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used to represent SoX combining types.
-- 

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Combine where

data Combine = C | S | MX | MP | MG | ML deriving Eq

instance Show Combine where
  show C = "--combine concatenate"
  show S = "--combine sequence"
  show MX = "--combine mix"
  show MP = "--combine mix-power"
  show MG = "--combine merge"
  show ML = "--combine multiply"

showC1 :: Combine -> [String]
showC1 = words . show
