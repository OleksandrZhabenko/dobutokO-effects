-- |
-- Module      :  DobutokO.Sound.Effects.Pitch
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the \"pitch\" SoX effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE FlexibleInstances #-}

module DobutokO.Sound.Effects.Pitch where


import Numeric (showFFloat)
import DobutokO.Sound.Effects.Segment

data Pitch a b c = Pt2 a b | Pt3 a b c deriving Eq

instance Show (Pitch Qdash Float Segm) where
  show (Pt2 x y) = mconcat ["pitch ", show x,showFFloat Nothing y " "]
  show (Pt3 x y z) = mconcat ["pitch ", show x, showFFloat Nothing y " ", show z]

type Ptch = Pitch Qdash Float Segm

pitchC :: Pitch a b c -> String
pitchC (Pt2 _ _) = "Pt2"
pitchC (Pt3 _ _ _) = "Pt3"

pitch1 :: Pitch a b c -> a
pitch1 (Pt2 x _) = x
pitch1 (Pt3 x _ _) = x

pitch2 :: Pitch a b c -> b
pitch2 (Pt2 _ y) = y
pitch2 (Pt3 _ y _) = y

pitch3 :: Pitch a b c -> Maybe c
pitch3 (Pt3 _ _ z) = Just z
pitch3 _ = Nothing

pitchSet1 :: a -> Pitch a b c -> Pitch a b c
pitchSet1 x (Pt2 _ y) = Pt2 x y
pitchSet1 x (Pt3 _ y z) = Pt3 x y z

pitchSet2 :: b -> Pitch a b c -> Pitch a b c
pitchSet2 y (Pt2 x _) = Pt2 x y
pitchSet2 y (Pt3 x _ z) = Pt3 x y z

pitchSet3 :: c -> Pitch a b c -> Pitch a b c
pitchSet3 z (Pt3 x y _) = Pt3 x y z
pitchSet3 z (Pt2 x y) = Pt3 x y z

showPtchQ :: Ptch -> [String]
showPtchQ = words . show
