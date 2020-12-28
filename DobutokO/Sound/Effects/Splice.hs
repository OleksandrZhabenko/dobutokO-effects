-- |
-- Module      :  DobutokO.Sound.Effects.Splice
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"splice\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Splice where

import Data.List (intersperse)
import DobutokO.Sound.Effects.Timespec
import DobutokO.Sound.One (One3(..))


data SpliceP = N0 | H | T | Q deriving Eq

instance Show SpliceP where
  show H = "-h "
  show T = "-t "
  show Q = "-q "
  show _ = ""

data Splice2 a b = SL a [One3 b] deriving Eq

instance Show (Splice2 SpliceP TSpecification) where
  show (SL x ys) 
    | null ys = ""
    | otherwise = mconcat ["splice ", show x, mconcat . intersperse " " . map show $ ys]

type Splice = Splice2 SpliceP TSpecification

splice21 :: Splice2 a b -> a
splice21 (SL x _) = x

splice22 :: Splice2 a b -> [One3 b]
splice22 (SL _ ys) = ys

splice2Set1 :: a -> Splice2 a b -> Splice2 a b
splice2Set1 x (SL _ ys) = SL x ys

splice2Set2 :: [One3 b] -> Splice2 a b -> Splice2 a b
splice2Set2 ys (SL x _) = SL x ys

showSLQ :: Splice -> [String]
showSLQ = words . show 
