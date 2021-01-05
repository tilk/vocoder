{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Vocoder.Dunai where

import Data.MonadicStreamFunction
import Data.Tuple(swap)
import Data.Maybe(fromMaybe)
import Data.Foldable
import qualified Data.Vector.Storable as V
import qualified Data.Vector as VV
import Vocoder

volumeFix :: Monad m => VocoderParams -> MSF m STFTFrame STFTFrame
volumeFix par = arr $ V.map (* volumeCoeff par) *** id

analysis :: Monad m => VocoderParams -> Phase -> MSF m [Frame] [STFTFrame]
analysis par = mealy $ \a s -> swap $ analysisStage par s a

synthesis :: Monad m => VocoderParams -> Phase -> MSF m [STFTFrame] [Frame]
synthesis par = mealy $ \a s -> swap $ synthesisStage par s a

process :: Monad m => VocoderParams -> MSF m [STFTFrame] [STFTFrame] -> MSF m [Frame] [Frame]
process par msf = analysis par (zeroPhase par) >>> msf >>> synthesis par (zeroPhase par)

volumeFixS :: Monad m => VocoderParams -> MSF m Frame Frame
volumeFixS par = arr $ V.map (* volumeCoeff par)

processS :: Monad m => VocoderParams -> MSF m [STFTFrame] [STFTFrame] -> MSF m Frame Frame
processS par msf = (framesOfS (inputFrameLength par) (hopSize par) >>> process par msf) &&& arr V.length >>> sumFramesWithLengthS (hopSize par) >>> volumeFixS par

data P a = P {-# UNPACK #-} !Length {-# UNPACK #-} !(V.Vector a)

mapP f g (P n c) = P (f n) (g c)

framesOfS :: forall a m. (V.Storable a, Num a, Monad m) => Length -> HopSize -> MSF m (V.Vector a) [V.Vector a]
framesOfS chunkSize hopSize = mealy f $ VV.fromList $ map (return . flip V.replicate 0) [hopSize, hopSize*2 .. chunkSize-1]
    where
    f :: V.Vector a -> VV.Vector [V.Vector a] -> ([V.Vector a], VV.Vector [V.Vector a])
    f next q = (outs, q'')
        where
        len = V.length next
        newChunks = VV.generate (len `div` hopSize) $ (return . flip V.drop next . (len -) . (* hopSize) . (+1))
        q' = newChunks VV.++ fmap (next :) q
        (q'', r) = VV.splitAt ((chunkSize-1) `div` hopSize) q'
        outs = VV.toList $ VV.reverse $ fmap (V.take chunkSize . V.concat . reverse) r

sumFramesS :: forall a m. (V.Storable a, Num a, Monad m) => Length -> HopSize -> MSF m [V.Vector a] (V.Vector a)
sumFramesS chunkSize hopSize = arr (id &&& const chunkSize) >>> sumFramesWithLengthS hopSize

sumFramesWithLengthS :: forall a m. (V.Storable a, Num a, Monad m) => HopSize -> MSF m ([V.Vector a], Length) (V.Vector a)
sumFramesWithLengthS hopSize = mealy f []
    where
    f :: ([V.Vector a], Length) -> [P a] -> (V.Vector a, [P a])
    f (nexts, chunkSize) q = (nextv, q'')
        where
        ith i (P n c0) = fromMaybe 0 $ c0 V.!? (i - n)
        q' = q ++ zipWith P [0, hopSize..] nexts
        nextv = V.generate chunkSize $ \i -> sum $ fmap (ith i) q'
        q'' = map (mapP (+ (-chunkSize)) id) $ dropWhile (\(P n c) -> V.length c + n <= chunkSize) q'

