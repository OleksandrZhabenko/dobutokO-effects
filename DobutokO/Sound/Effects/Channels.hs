-- |
-- Module      :  DobutokO.Sound.Effects.Channels
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"channels\" SoX effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Channels where

data Chans a = Chans a deriving Eq

instance Show (Chans Int) where
  show (Chans n) 
    | compare n 0 == GT = "channels " ++ show n
    | otherwise = ""

type ChansI = Chans Int

channels1 :: Chans a -> a
channels1 (Chans x) = x

channelsSet1 :: a -> Chans a
channelsSet1 = Chans 

showChnQ :: ChansI -> [String]
showChnQ = words . show
