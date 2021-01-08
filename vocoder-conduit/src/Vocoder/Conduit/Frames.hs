{-| 
    Module      : Vocoder.Conduit.Frames
    Description : Frame processing
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2
-}
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Vocoder.Conduit.Frames (
    framesOfE,
    sumFramesE
    ) where

import Control.Arrow
import Control.Monad
import Data.Conduit
import Data.Foldable
import Data.MonoTraversable
import Data.Maybe(fromMaybe)
import qualified Data.Sequences as Seq

-- | Splits a chunked input stream into overlapping frames of constant size
--   suitable for STFT processing.
framesOfE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Seq.Index seq -> ConduitT seq seq m ()
framesOfE chunkSize hopSize = process 0 []
    where
        process !sofar q = do
            next <- await
            case next of
                Nothing -> case q of
                    [] -> return ()
                    h : _ -> leftover (mconcat . reverse . snd $ h)
                Just next' -> do
                    let len = Seq.lengthIndex next'
                    let newChunks = map ((len -) &&& (Seq.singleton . flip Seq.drop next')) [sofar, sofar + hopSize .. len-1]
                    let q' = fmap ((len +) *** (next' :)) q ++ newChunks
                    let (r, q'') = span ((>= chunkSize) . fst) q'
                    forM_ (map (Seq.take chunkSize . mconcat . reverse . snd) $ toList r) yield
                    process ((sofar - len) `mod` hopSize) q''

-- | Builds a chunked output stream from a stream of overlapping frames.
sumFramesE :: (Monad m, Seq.IsSequence seq, Num (Element seq)) => Seq.Index seq -> Seq.Index seq -> ConduitT seq seq m ()
sumFramesE chunkSize hopSize = process 0 []
    where
        ith i (n, c0) = fromMaybe 0 $ Seq.index c0 (i - n)
        publish q = yield $ Seq.fromList $ map (\i -> sum $ fmap (ith i) q) [0 .. chunkSize-1]
        publishRest q | null q    = return ()
                      | otherwise = publish q >> publishRest (nextq q)
        nextq q = fmap ((+ (-chunkSize)) *** id) $ dropWhile (\(n, c) -> Seq.lengthIndex c + n <= chunkSize) q
        process2 sofar q
            | sofar >= chunkSize = do
                publish q
                process2 (sofar - chunkSize) $ nextq q
            | otherwise = process (sofar + hopSize) q
        process sofar q = do
            next <- await
            case next of
                Nothing -> publishRest q
                Just next' -> process2 sofar (q ++ [(sofar, next')])


