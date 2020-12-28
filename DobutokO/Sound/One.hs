-- |
-- Module      :  DobutokO.Sound.One
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.One where

data One2 a = O21 a | O22 a a deriving Eq

one2C :: One2 a -> String
one2C (O21 _) = "O21"
one2C (O22 _ _) = "O22"

one21 :: One2 a -> [a]
one21 (O21 x) = [x]
one21 (O22 x y) = [x,y]

one2Set1 :: a -> One2 a -> One2 a
one2Set1 x (O21 _) = O21 x
one2Set1 x (O22 _ y) = O22 x y

one2Set2 :: a -> One2 a -> One2 a
one2Set2 y (O21 x) = O22 x y
one2Set2 y (O22 x _) = O22 x y

one2toList :: One2 a -> [a]
one2toList (O21 x) = [x]
one2toList (O22 x y) = [x,y]

data One3 a = O31 a | O32 a a | O33 a a a deriving Eq

one3C :: One3 a -> String
one3C (O31 _) = "O31"
one3C (O32 _ _) = "O32"
one3C (O33 _ _ _) = "O33"

one31 :: One3 a -> [a]
one31 (O31 x) = [x]
one31 (O32 x y) = [x,y]
one31 (O33 x y z) = [x,y,z]

one3Set1 :: a -> One3 a -> One3 a
one3Set1 x (O31 _) = O31 x
one3Set1 x (O32 _ y) = O32 x y
one3Set1 x (O33 _ y z) = O33 x y z

one3Set2 :: a -> One3 a -> One3 a
one3Set2 y (O31 x) = O32 x y
one3Set2 y (O32 x _) = O32 x y
one3Set2 y (O33 x _ z) = O33 x y z

one3Set3 :: a -> One3 a -> Maybe (One3 a)
one3Set3 z (O32 x y) = Just (O33 x y z)
one3Set3 z (O33 x y _) = Just (O33 x y z)
one3Set3 _ _ = Nothing

one3toList :: One3 a -> [a]
one3toList (O31 x) = [x]
one3toList (O32 x y) = [x, y]
one3toList (O33 x y z) = [x, y, z]
