{-| 
    Module      : Vocoder.Conduit.Frames
    Description : Frame processing
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2
-}
{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Vocoder.Conduit.Frames (
    framesOfE,
    genFramesOfE,
    sumFramesE
    ) where

import Control.Arrow
import Data.Conduit
import Data.MonoTraversable
import Data.Maybe(fromMaybe)
import qualified Data.Sequences as Seq

-- | Splits a chunked input stream into overlapping frames of constant size
--   suitable for STFT processing.
framesOfE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Seq.Index seq -> ConduitT seq seq m ()
framesOfE chunkSize hopSize = genFramesOfE chunkSize hopSize (Seq.fromList []) >> return ()

-- | More general version of framesOfE, suitable for processing multiple inputs.
genFramesOfE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Seq.Index seq -> seq -> ConduitT seq seq m seq
genFramesOfE chunkSize hopSize q = do
    mnextv <- await
    case mnextv of
        Nothing -> return q
        Just nextv -> do
            let newBuf = q `mappend` nextv
            let newBufLen = Seq.lengthIndex newBuf
            mapM_ yield [Seq.take chunkSize $ Seq.drop k newBuf
                        | k <- [0, hopSize .. newBufLen - chunkSize]]
            let dropcnt = ((newBufLen - chunkSize) `div` hopSize) * hopSize + hopSize
            let q' = Seq.drop dropcnt newBuf
            genFramesOfE chunkSize hopSize q'

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


