{-| 
    Module      : Vocoder.Dunai
    Description : Phase vocoder in Dunai
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module wraps phase vocoder algorithms for use in Dunai and Rhine.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Vocoder.Dunai (
    volumeFix,
    analysis,
    synthesis,
    processFrames,
    process,
    framesOfS,
    sumFramesS,
    sumFramesWithLengthS
    ) where

import Data.MonadicStreamFunction
import Data.Tuple(swap)
import Data.Maybe(fromMaybe)
import qualified Data.Vector.Storable as V
import Vocoder

-- | Perform the phase vocoder analysis phase.
analysis :: (Traversable t, Monad m) => VocoderParams -> Phase -> MSF m (t Frame) (t STFTFrame)
analysis par = mealy $ \a s -> swap $ analysisStage par s a

-- | Perform the phase vocoder synthesis phase.
synthesis :: (Traversable t, Monad m) => VocoderParams -> Phase -> MSF m (t STFTFrame) (t Frame)
synthesis par = mealy $ \a s -> swap $ synthesisStage par s a

-- | Perform frequency domain processing on overlapping frames.
processFrames :: (Traversable t, Monad m) => VocoderParams -> MSF m (t STFTFrame) (t STFTFrame) -> MSF m (t Frame) (t Frame)
processFrames par msf = analysis par (zeroPhase par) >>> msf >>> synthesis par (zeroPhase par)

-- | Corrects for volume change introduced by STFT processing.
volumeFix :: Monad m => VocoderParams -> MSF m Frame Frame
volumeFix par = arr $ V.map (* volumeCoeff par)

-- | Perform frequency domain processing on a chunked stream. 
--   The chunks' size must be a multiple of the vocoder's hop size.
process :: Monad m => VocoderParams -> MSF m [STFTFrame] [STFTFrame] -> MSF m Frame Frame
process par msf = (framesOfS (vocInputFrameLength par) (vocHopSize par) >>> processFrames par msf) &&& arr V.length 
               >>> sumFramesWithLengthS (vocHopSize par) >>> volumeFix par

data P a = P {-# UNPACK #-} !Length {-# UNPACK #-} !(V.Vector a)

mapP :: (Length -> Length) -> (V.Vector a1 -> V.Vector a2) -> P a1 -> P a2
mapP f g (P n c) = P (f n) (g c)

-- | Splits a chunked input stream into overlapping frames of constant size
--   suitable for STFT processing.
--   The input and output chunks' size must be a multiple of the vocoder's hop size.
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

-- | Builds a chunked output stream from a stream of overlapping frames.
--   The input and output chunks's size must be a multiple of the vocoder's hop size.
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

