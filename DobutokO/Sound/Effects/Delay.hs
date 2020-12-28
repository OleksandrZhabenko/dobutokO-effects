-- |
-- Module      :  DobutokO.Sound.Effects.Delay
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"delay\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Delay where

import Data.List (intersperse)
import DobutokO.Sound.Effects.Timespec

data Delay a = D [a] deriving Eq

instance Show (Delay TSpecification) where
  show (D xs) 
    | null xs = []
    | otherwise = mconcat ["delay ",mconcat . intersperse " " . map show $ xs]

type Dlay = Delay TSpecification

delay1 :: Dlay -> [TSpecification]
delay1 (D xs) = xs

delaySet1 :: [TSpecification] -> Dlay
delaySet1 = D

showDlQ :: Dlay -> [String]
showDlQ = words . show
