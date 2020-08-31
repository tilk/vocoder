module VocoderConduit where

import Data.Conduit
import qualified Data.Conduit.List as DCL
import Control.Arrow
import Vocoder

analysis0 :: Monad m => Vocoder_params -> Phase -> ConduitT (HopSize, Frame) (HopSize, SFT_block) m Phase
analysis0 par ph = do
    v <- await
    case v of
        Just (h, f) -> do
            let o = smart_fft_with_plan par f
            let (ph', r) = analysis_step (frame_length par) ph (h, o)
            yield r
            analysis0 par ph'
        Nothing -> return ph

synthesis0 :: Monad m => Vocoder_params -> Phase -> ConduitT (HopSize, SFT_block) (HopSize, Frame) m Phase
synthesis0 par ph = do
    v <- await
    case v of
        Just i -> do
            let (ph', (h, r)) = synthesis_step ph i
            let o = smart_ifft_with_plan par r
            yield (h, o)
            synthesis0 par ph'
        Nothing -> return ph

analysis :: Monad m => Vocoder_params -> Phase -> ConduitT (HopSize, Frame) (HopSize, SFT_block) m Phase
analysis par ph = DCL.map (id *** smart_fft_with_plan par) .| DCL.mapAccum (flip $ analysis_step (frame_length par)) ph

synthesis :: Monad m => Vocoder_params -> Phase -> ConduitT (HopSize, SFT_block) (HopSize, Frame) m Phase
synthesis par ph = DCL.mapAccum (flip synthesis_step) ph `fuseUpstream` DCL.map (id *** smart_ifft_with_plan par)

{-
process :: Monad m => Vocoder_params -> (Phase, Phase) -> ConduitT SFT_block SFT_block m r -> ConduitT Frame Frame m (r, (Phase, Phase))
process par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysis par p1 `fuseBoth` c `fuseBoth` synthesis par p2
-}

