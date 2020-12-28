-- |
-- Module      :  DobutokO.Sound.ToRange
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Auxiliary module to re-position the number to the needed range.
-- 

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.ToRange where

toRange :: Float -> Float -> Float
toRange range x 
 | range /= 0.0 = (x / range - fromIntegral (truncate (x / range))) * range
 | otherwise = error "DobutokO.Sound.ToRange.toRange: Zero range. "

