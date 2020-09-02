{-# LANGUAGE BangPatterns #-}
module ConduitUtils where

import Control.Arrow
import Control.Monad
import Data.Conduit
import Data.Foldable
import Data.MonoTraversable
import qualified Data.Conduit.Internal as DCI
import qualified Data.Sequence as DS
import qualified Data.Sequences as Seq
import qualified Data.Conduit.List as DCL

cfirst :: Monad m => a -> ConduitT i o m r -> ConduitT (i, a) (o, a) m r
cfirst a0 (DCI.ConduitT c0) = DCI.ConduitT $ \rest -> let
    go a (DCI.HaveOutput p o) = DCI.HaveOutput (go a p) (o, a)
    go a (DCI.NeedInput p c) = DCI.NeedInput (\(i, a') -> go a' $ p i) (go a . c)
    go a (DCI.Done r) = rest r
    go a (DCI.PipeM mp) = DCI.PipeM (liftM (go a) mp)
    go a (DCI.Leftover p i) = DCI.Leftover (go a p) (i, a)
    in go a0 (c0 DCI.Done)

framesOfE :: (Monad m, Seq.IsSequence seq) => Seq.Index seq -> Seq.Index seq -> ConduitT seq seq m ()
framesOfE chunkSize hopSize = process 0 DS.empty
    where
        process !sofar q = do
            next <- await
            case next of
                Nothing -> case q of
                    DS.Empty -> return ()
                    h DS.:<| _ -> leftover (mconcat . reverse . snd $ h)
                Just next' -> do
                    let len = Seq.lengthIndex next'
                    let newChunks = map ((len -) &&& (Seq.singleton . flip Seq.drop next')) [sofar, sofar + hopSize .. len-1]
                    let q' = fmap ((len +) *** (next' :)) q DS.>< DS.fromList newChunks
                    let (r, q'') = DS.spanl ((>= chunkSize) . fst) q'
                    forM (map (Seq.take chunkSize . mconcat . reverse . snd) $ toList r) yield
                    process ((sofar - len) `mod` hopSize) q''

sumFramesWithE :: (Monad m, Seq.IsSequence seq) => Element seq -> (seq -> seq -> seq) -> Seq.Index seq -> Seq.Index seq -> ConduitT seq seq m ()
sumFramesWithE zero add chunkSize hopSize = process 0 DS.empty
    where
        prep (n, c0) = fLeft $ fRight c0
            where
                fLeft c  | n <= 0    = Seq.drop (-n) c
                         | otherwise = Seq.replicate n zero <> c
                fRight c | r <= 0    = Seq.dropEnd (-r) c
                         | otherwise = c <> Seq.replicate r zero
                r = chunkSize - n - Seq.lengthIndex c0
        publish q = unless (DS.null q) $ yield $ foldr1 add $ fmap prep q
        process sofar q 
            | sofar >= chunkSize = do
                publish q
                let q' = fmap ((+ (-chunkSize)) *** id) $ DS.dropWhileL (\(n, c) -> Seq.lengthIndex c + n <= chunkSize) q
                process (sofar - chunkSize) q'
            | otherwise = do
                next <- await
                case next of
                    Nothing -> publish q
                    Just next' -> 
                        process (sofar + hopSize) (q DS.|> (sofar, next'))


