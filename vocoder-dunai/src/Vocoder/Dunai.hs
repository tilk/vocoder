{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Vocoder.Dunai where

import Data.MonadicStreamFunction
import Data.Tuple(swap)
import Data.Maybe(fromMaybe)
import qualified Data.Vector.Storable as V
import Vocoder

volumeFix :: Monad m => VocoderParams -> MSF m STFTFrame STFTFrame
volumeFix par = arr $ V.map (* volumeCoeff par) *** id

analysis :: (Traversable t, Monad m) => VocoderParams -> Phase -> MSF m (t Frame) (t STFTFrame)
analysis par = mealy $ \a s -> swap $ analysisStage par s a

synthesis :: (Traversable t, Monad m) => VocoderParams -> Phase -> MSF m (t STFTFrame) (t Frame)
synthesis par = mealy $ \a s -> swap $ synthesisStage par s a

process :: (Traversable t, Monad m) => VocoderParams -> MSF m (t STFTFrame) (t STFTFrame) -> MSF m (t Frame) (t Frame)
process par msf = analysis par (zeroPhase par) >>> msf >>> synthesis par (zeroPhase par)

volumeFixS :: Monad m => VocoderParams -> MSF m Frame Frame
volumeFixS par = arr $ V.map (* volumeCoeff par)

processS :: Monad m => VocoderParams -> MSF m [STFTFrame] [STFTFrame] -> MSF m Frame Frame
processS par msf = (framesOfS (inputFrameLength par) (hopSize par) >>> process par msf) &&& arr V.length >>> sumFramesWithLengthS (hopSize par) >>> volumeFixS par

data P a = P {-# UNPACK #-} !Length {-# UNPACK #-} !(V.Vector a)

mapP f g (P n c) = P (f n) (g c)

framesOfS :: forall a m. (V.Storable a, Num a, Monad m) => Length -> HopSize -> MSF m (V.Vector a) [V.Vector a]
framesOfS chunkSize hopSize = mealy f $ V.replicate bufLen 0
    where
    bufHops = (chunkSize-1) `div` hopSize
    bufLen = bufHops * hopSize
    f :: V.Vector a -> V.Vector a -> ([V.Vector a], V.Vector a)
    f nextv q = (outs, q')
        where
        len = V.length nextv
        newBuf = q V.++ nextv
        q' = V.drop len newBuf
        outs = [V.take chunkSize $ V.drop (k * hopSize) newBuf | k <- [0 .. len `div` hopSize - 1]]

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

