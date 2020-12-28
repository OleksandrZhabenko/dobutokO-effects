-- |
-- Module      :  DobutokO.Sound.Effects.Remix
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"remix\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Remix where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import Data.List (intersperse)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Vol3 a = P | I | V | P2 a | I2 a | V2 a deriving Eq

instance (Show a, RealFloat a) => Show (Vol3 a) where
  show P = "p0"
  show I = "i0"
  show V = "v1"
  show (P2 a) = 'p':showFFloat Nothing a ""
  show (I2 a) = 'i':showFFloat Nothing a ""
  show (V2 a) = 'v':showFFloat Nothing a ""

vol31 :: Vol3 Float -> Float
vol31 P = 0.0
vol31 I = 0.0
vol31 V = 1.0
vol31 (P2 x) = x
vol31 (I2 x) = x
vol31 (V2 x) = x

vol3Set1 :: Float -> Vol3 Float -> Vol3 Float
vol3Set1 x P 
 | x == 0.0 = P
 | otherwise = P2 x
vol3Set1 x I 
 | x == 0.0 = I
 | otherwise = I2 x
vol3Set1 x V 
 | x == 1.0 = V
 | otherwise = V2 x
vol3Set1 x (P2 _) = P2 x
vol3Set1 x (I2 _) = I2 x
vol3Set1 x (V2 _) = V2 x

type Vol3F = Vol3 Float

data IChannel a b = ICh a (Vol3 b) deriving Eq

ichannel1 :: IChannel a b -> a
ichannel1 (ICh x _) = x

ichannel2 :: Vol3F -> IChannel a Float -> Vol3F
ichannel2 P (ICh _ y) = y
ichannel2 I (ICh _ y) = y
ichannel2 V (ICh _ y) = y
ichannel2 (P2 _) (ICh _ y) = P2 (vol31 y)
ichannel2 (I2 _) (ICh _ y) = I2 (vol31 y)
ichannel2 (V2 _) (ICh _ y) = V2 (vol31 y)

ichannel2C :: IChannel a b -> String
ichannel2C (ICh _ P) = "P"
ichannel2C (ICh _ I) = "I"
ichannel2C (ICh _ V) = "V"
ichannel2C (ICh _ (P2 _)) = "P2"
ichannel2C (ICh _ (I2 _)) = "I2"
ichannel2C (ICh _ (V2 _)) = "V2"

ichannel21 :: IChannel a Float -> Float
ichannel21 (ICh _ y) = vol31 y

ichannelSet1 :: a -> IChannel a b -> IChannel a b
ichannelSet1 x (ICh _ y) = ICh x y

ichannelSet2 :: Vol3 b -> IChannel a b -> IChannel a b
ichannelSet2 y (ICh x _) = ICh x y

type IChanF = IChannel Int Float

instance (Show a, Integral a, Show b, RealFloat b) => Show (IChannel a b) where
  show (ICh x y) = mconcat [show x, show y]

data OChannel a = OCh [a] deriving Eq

instance (Show a) => Show (OChannel a) where
  show (OCh []) = []
  show (OCh [x]) = show x
  show (OCh ys) = mconcat . intersperse "," . map show $ ys

ochannel1 :: OChannel a -> [a]
ochannel1 (OCh xs) = xs

ochannelSet1 :: [a] -> OChannel a -> OChannel a
ochannelSet1 xs _ = OCh xs

type OChanF = OChannel IChanF

data MixSpec = A | M | D deriving Eq

instance Show MixSpec where
  show A = "-a "
  show M = "-m "
  show D = []

data Remix a b = Rmx | Rmix a [b] deriving Eq

instance Show (Remix MixSpec OChanF) where
  show (Rmix x ys) = mconcat ["remix ",show x, mconcat . intersperse " " . map show $ ys]
  show Rmx = "remix -"

remixC :: Remix a b -> String
remixC Rmx = "Rmx"
remixC (Rmix _ _) = "Rmix"

remix1 :: Remix a b -> Maybe a
remix1 Rmx = Nothing
remix1 (Rmix x _) = Just x

remix2 :: Remix a b -> [b]
remix2 Rmx = []
remix2 (Rmix _ xs) = xs

type ReMix = Remix MixSpec OChanF

remixSet1 :: MixSpec -> ReMix -> ReMix
remixSet1 _ Rmx = Rmx
remixSet1 y (Rmix _ xs) = Rmix y xs

remixSet2 :: [OChanF] -> ReMix -> ReMix
remixSet2 ys Rmx = Rmix D ys
remixSet2 ys (Rmix x _) = Rmix x ys

showRmQ :: ReMix -> [String]
showRmQ = words . show 

