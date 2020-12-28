-- |
-- Module      :  DobutokO.Sound.Effects.Reverb
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to create experimental music. 
-- Can be used for applying the SoX \"reverb\" and \"reverse\" effects. 
-- 

{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE CPP, FlexibleInstances #-}

module DobutokO.Sound.Effects.Reverb where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Numeric (showFFloat)
import Data.List (intersperse)
import qualified DobutokO.Sound.Frequency as FQ
import DobutokO.Sound.ToRange

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data Four = O4 | T4 | H4 | F4 deriving Eq

data Reverb a b c d =  Rvrb a b [c] d deriving Eq

instance Show (Reverb Four FQ.Di Float Int) where
  show (Rvrb variant wet xs n) 
   | compare n 0 == LT = error $ "DobutokO.Sound.Effects.Reverb.show is not defined for the value of the last argument " ++ show n
   | otherwise = 
      let (zs, ks) = splitAt 4 xs in 
        let ys = map (toRange 100.0) zs in 
             if null ks 
               then case variant of
                       O4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys]
                       T4 -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," reverse"]
                       H4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," reverse"]
                       _  -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys]
               else let r5 = head ks in if null . tail $ ks then 
                      case variant of
                        O4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys, " ",
                          showFFloat (Just n) r5 ""]
                        T4 -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                          showFFloat (Just n) r5 ""," reverse"]
                        H4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                          showFFloat (Just n) r5 ""," reverse"]
                        _  -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                          showFFloat (Just n) r5 ""]
                     else let r60 = last ks in 
                            let r6 = (r60 / 120.0 - (fromIntegral . truncate $ r60 / 120.0)) * 120.0 in
                                 case variant of 
                                   O4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys, " ",
                                     showFFloat (Just n) r5 ""," ",showFFloat (Just n) r6 ""]
                                   T4 -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                                     showFFloat (Just n) r5 " ",showFFloat (Just n) r6 " reverse"]
                                   H4 -> mconcat ["reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                                     showFFloat (Just n) r5 " ",showFFloat (Just n) r6 " reverse"]
                                   _  -> mconcat ["reverse reverb ", if wet == FQ.O then "-w " else "", mconcat . intersperse " " . map (\x -> showFFloat (Just n) x " ") $ ys," ",
                                     showFFloat (Just n) r5 " ",showFFloat (Just n) r6 ""]

reverb1 :: Reverb a b c d -> a
reverb1 (Rvrb x _ _ _) = x

reverb2 :: Reverb a b c d -> b
reverb2 (Rvrb _ y _ _) = y

reverb3 :: Reverb a b c d -> [c]
reverb3 (Rvrb _ _ zs _) = take 6 zs

reverb4 :: Reverb a b c d -> d
reverb4 (Rvrb _ _ _ t) = t

type ReverbE = Reverb Four FQ.Di Float Int

reverb3E :: Int -> ReverbE -> Float
reverb3E n x 
 | compare n 0 == GT && compare n 7 == LT = if null . drop (n - 1) . reverb3 $ x then 50.0 * fromIntegral ((((n - 1) `quot` 2) + 1) `rem` 3) else reverb3 x !! (n - 1)
 | otherwise = error "DobutokO.Sound.Effects.Reverb.reverb3E: Not defined parameter. "

reverbSet1 :: a -> Reverb a b c d -> Reverb a b c d
reverbSet1 x (Rvrb _ y zs t) = Rvrb x y zs t

reverbSet2 :: b -> Reverb a b c d -> Reverb a b c d
reverbSet2 y (Rvrb x _ zs t) = Rvrb x y zs t

reverbSet3 :: [c] -> Reverb a b c d -> Reverb a b c d
reverbSet3 zs (Rvrb x y _ t) = Rvrb x y (take 6 zs) t

reverbSet4 :: d -> Reverb a b c d -> Reverb a b c d
reverbSet4 t (Rvrb x y zs _) = Rvrb x y zs t

reverbSet3E :: Int -> Float -> ReverbE -> ReverbE
reverbSet3E n y x 
 | compare n 0 == GT && compare n 5 == LT = Rvrb (reverb1 x) (reverb2 x) (mconcat [take (n - 1) . reverb3 $ x, [toRange 100.0 y], drop n . reverb3 $ x]) (reverb4 x)
 | n == 5 = Rvrb (reverb1 x) (reverb2 x) (mconcat [take 4 . reverb3 $ x, [y], drop 5 . reverb3 $ x]) (reverb4 x)
 | n == 6 = Rvrb (reverb1 x) (reverb2 x) (mconcat [take 5 . reverb3 $ x, [toRange 120.0 y]]) (reverb4 x)
 | otherwise = error "DobutokO.Sound.Effects.Reverb.reverbSet3E: The first argument is out of range [1..6]. "

showQReverb :: ReverbE -> [String]
showQReverb = words . show
   
