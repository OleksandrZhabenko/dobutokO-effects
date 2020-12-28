-- |
-- Module      :  DobutokO.Sound.Effects.Trim
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"trim\" effect with the needed time specifications. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Trim where

import DobutokO.Sound.Effects.Timespec


data Trim a = Trim a deriving Eq

instance Show (Trim TSpecification) where
  show (Trim x) = mconcat ["trim ",show x]

trim1 :: Trim a -> a
trim1 (Trim x) = x

trimSet1 :: a -> Trim a
trimSet1 x = Trim x

type Trim1 = Trim TSpecification

showTrimQ :: Trim1 -> [String]
showTrimQ = words . show
