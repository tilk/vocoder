module Vocoder.Rhine where

import FRP.Rhine
import Data.Tuple(swap)
import Data.Maybe(fromMaybe)
import Data.Foldable
import qualified Data.Vector.Storable as V
import qualified Data.Sequence as DS
import Control.Arrow
import Vocoder

volumeFix :: Monad m => VocoderParams -> MSF m STFTFrame STFTFrame
volumeFix par = arr $ V.map (* volumeCoeff par) *** id

analysis :: Monad m => VocoderParams -> Phase -> MSF m [Frame] [STFTFrame]
analysis par = mealy $ \a s -> swap $ analysisStage par s a

synthesis :: Monad m => VocoderParams -> Phase -> MSF m [STFTFrame] [Frame]
synthesis par = mealy $ \a s -> swap $ synthesisStage par s a

framesOfS :: Monad m => Length -> HopSize -> MSF m Frame [Frame]
framesOfS chunkSize hopSize = mealy f $ DS.fromList $ map (id &&& return . flip V.replicate 0) [hopSize, hopSize*2 .. chunkSize-1]
    where
    f :: Frame -> DS.Seq (Length, [Frame]) -> ([Frame], DS.Seq (Length, [Frame]))
    f next q = (outs, q'')
        where
        len = V.length next
        newChunks = map ((len -) &&& (return . flip V.drop next)) [0, hopSize .. len-1]
        q' = fmap ((+ len) *** (next :)) q DS.>< DS.fromList newChunks
        (r, q'') = DS.spanl ((>= chunkSize) . fst) q'
        outs = map (V.take chunkSize . mconcat . reverse . snd) $ toList r

sumFramesS :: Monad m => Length -> HopSize -> MSF m [Frame] Frame
sumFramesS chunkSize hopSize = mealy f DS.empty
    where
    f :: [Frame] -> DS.Seq (Length, Frame) -> (Frame, DS.Seq (Length, Frame))
    f nexts q = (next, q'')
        where
        ith i (n, c0) = fromMaybe 0 $ c0 V.!? (i - n)
        q' = q DS.>< DS.fromList (zip [0, hopSize..] nexts)
        next = V.generate chunkSize $ \i -> sum $ fmap (ith i) q'
        q'' = fmap ((+ (-chunkSize)) *** id) $ DS.dropWhileL (\(n, c) -> V.length c + n <= chunkSize) q'


