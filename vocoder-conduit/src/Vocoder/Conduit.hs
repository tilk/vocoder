module Vocoder.Conduit where

import Data.Conduit
import qualified Data.Conduit.List as DCL
import qualified Data.List.NonEmpty as DLN
import Control.Arrow
import Vocoder

analysis0 :: Monad m => Vocoder_params -> Phase -> ConduitT Frame SFT_block m Phase
analysis0 par ph = do
    v <- await
    case v of
        Just f -> do
            let o = smart_fft_with_plan par f
            let (ph', r) = analysis_step (hop_size par) (frame_length par) ph o
            yield r
            analysis0 par ph'
        Nothing -> return ph

synthesis0 :: Monad m => Vocoder_params -> Phase -> ConduitT SFT_block Frame m Phase
synthesis0 par ph = do
    v <- await
    case v of
        Just i -> do
            let (ph', r) = synthesis_step (hop_size par) ph i
            yield $ smart_ifft_with_plan par r
            synthesis0 par ph'
        Nothing -> return ph

analysis :: Monad m => Vocoder_params -> Phase -> ConduitT Frame SFT_block m Phase
analysis par ph = DCL.map (smart_fft_with_plan par) .| DCL.mapAccum (flip $ analysis_step (hop_size par) (frame_length par)) ph

synthesis :: Monad m => Vocoder_params -> Phase -> ConduitT SFT_block Frame m Phase
synthesis par ph = DCL.mapAccum (flip $ synthesis_step (hop_size par)) ph `fuseUpstream` DCL.map (smart_ifft_with_plan par)

process :: Monad m => Vocoder_params -> (Phase, Phase) -> ConduitT SFT_block SFT_block m r -> ConduitT Frame Frame m (r, (Phase, Phase))
process par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysis par p1 `fuseBoth` c `fuseBoth` synthesis par p2

app_help :: Applicative f => (a -> s -> (s, b)) -> f a -> f s -> (f s, f b)
app_help f a b = DLN.unzip $ fmap (uncurry f) ((,) <$> a <*> b)

analysisF :: (Applicative f, Monad m) => Vocoder_params -> f Phase -> ConduitT (f Frame) (f SFT_block) m (f Phase)
analysisF par ph = DCL.map (fmap $ smart_fft_with_plan par) .| DCL.mapAccum (app_help $ flip $ analysis_step (hop_size par) (frame_length par)) ph

synthesisF :: (Applicative f, Monad m) => Vocoder_params -> f Phase -> ConduitT (f SFT_block) (f Frame) m (f Phase)
synthesisF par ph = DCL.mapAccum (app_help $ flip $ synthesis_step (hop_size par)) ph `fuseUpstream` DCL.map (fmap $ smart_ifft_with_plan par)

processF :: (Applicative f, Monad m) => Vocoder_params -> (f Phase, f Phase) -> ConduitT (f SFT_block) (f SFT_block) m r -> ConduitT (f Frame) (f Frame) m (r, (f Phase, f Phase))
processF par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysisF par p1 `fuseBoth` c `fuseBoth` synthesisF par p2

