-- |
-- Module      :  DobutokO.Sound.Effects.Vad
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"vad\" effect. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Vad where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import DobutokO.Sound.ToRange
import DobutokO.Sound.Effects.Misc (MscS(..),mscS1)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data VadP a = T1 a | T a | S1 a | G a | P1 a | B a | N a | N1 a | R a | F a | M1 a | M a | H1 a | L1 a | H a | L a deriving Eq

instance Show (VadP Float) where
  show (T1 x) = mconcat ["-t ", showFFloat Nothing (toRange 20.0 . abs $ x) " "]
  show (T x) = mconcat ["-T ", showFFloat Nothing (if compare (toRange 1.0 . abs $ x) 0.01 == LT then 0.01 else toRange 1.0 . abs $ x) " "]
  show (S1 x) = mconcat ["-s ", showFFloat Nothing (if compare (toRange 4.0 . abs $ x) 0.1 == LT then 0.1 else toRange 4.0 . abs $ x) " "]
  show (G x) = mconcat ["-g ", showFFloat Nothing (if compare (toRange 1.0 . abs $ x) 0.1 == LT then 0.1 else toRange 1.0 . abs $ x) " "]
  show (P1 x) = mconcat ["-p ", showFFloat Nothing (toRange 4.0 . abs $ x) " "]
  show (B x) = mconcat ["-b ", showFFloat Nothing (if compare (toRange 10.0 . abs $ x) 0.1 == LT then 0.1 else toRange 10.0 . abs $ x) " "]
  show (N x) = mconcat ["-N ", showFFloat Nothing (if compare (toRange 10.0 . abs $ x) 0.1 == LT then 0.1 else toRange 10.0 . abs $ x) " "]
  show (N1 x) = mconcat ["-n ", showFFloat Nothing (if compare (toRange 0.1 . abs $ x) 0.001 == LT then 0.001 else toRange 0.1 . abs $ x) " "]
  show (R x) = mconcat ["-r ", showFFloat Nothing (toRange 2.0 . abs $ x) " "]
  show (F x) = mconcat ["-f ", showFFloat Nothing (if compare (toRange 50.0 . abs $ x) 5.0 == LT then 5.0 else toRange 50.0 . abs $ x) " "]
  show (M1 x) = mconcat ["-m ", showFFloat Nothing (if compare (toRange 1.0 . abs $ x) 0.01 == LT then 0.01 else toRange 1.0 . abs $ x) " "]
  show (M x) = mconcat ["-M ", showFFloat Nothing (if compare (toRange 1.0 . abs $ x) 0.1 == LT then 0.1 else toRange 1.0 . abs $ x) " "]
  show (H1 x) = mconcat ["-h ", showFFloat Nothing (abs x) " "]
  show (L1 x) = mconcat ["-l ", showFFloat Nothing (abs x) " "]
  show (H x) = mconcat ["-H ", showFFloat Nothing (abs x) " "]
  show (L x) = mconcat ["-L ", showFFloat Nothing (abs x) " "]

type VadP1 = VadP Float

vadPC :: VadP a -> String
vadPC (T1 _) = "T1"
vadPC (T _) = "T"
vadPC (S1 _) = "S1"
vadPC (G _) = "G"
vadPC (P1 _) = "P1"
vadPC (B _) = "B"
vadPC (N _) = "N"
vadPC (N1 _) = "N1"
vadPC (R _) = "R"
vadPC (F _) = "F"
vadPC (M1 _) = "M1"
vadPC (M _) = "M"
vadPC (H1 _) = "H1"
vadPC (L1 _) = "L1"
vadPC (H _) = "H"
vadPC (L _) = "L"

vadP1 :: VadP a -> a
vadP1 (T1 x) = x
vadP1 (T x) = x
vadP1 (S1 x) = x
vadP1 (G x) = x
vadP1 (P1 x) = x
vadP1 (B x) = x
vadP1 (N x) = x
vadP1 (N1 x) = x
vadP1 (R x) = x
vadP1 (F x) = x
vadP1 (M1 x) = x
vadP1 (M x) = x
vadP1 (H1 x) = x
vadP1 (L1 x) = x
vadP1 (H x) = x
vadP1 (L x) = x

vadPSet1 :: a -> VadP a -> VadP a
vadPSet1 x (T1 _) = T1 x
vadPSet1 x (T _) = T x
vadPSet1 x (S1 _) = S1 x
vadPSet1 x (G _) = G x
vadPSet1 x (P1 _) = P1 x
vadPSet1 x (B _) = B x
vadPSet1 x (N _) = N x
vadPSet1 x (N1 _) = N1 x
vadPSet1 x (R _) = R x
vadPSet1 x (F _) = F x
vadPSet1 x (M1 _) = M1 x
vadPSet1 x (M _) = M x
vadPSet1 x (H1 _) = H1 x
vadPSet1 x (L1 _) = L1 x
vadPSet1 x (H _) = H x
vadPSet1 x (L _) = L x

data Vad1 a = VD (MscS a) deriving Eq

instance Show (Vad1 VadP1) where
  show (VD x) = mconcat ["vad ", show x]

type Vad = Vad1 VadP1

vad11 :: Vad1 a -> [a]
vad11 (VD x) = mscS1 x

vad1Set1 :: [a] -> Vad1 a -> Vad1 a
vad1Set1 xs (VD (Msc _)) = VD (Msc xs)

showVDQ :: Vad -> [String]
showVDQ = words . show 
