{-| 
    Module      : Vocoder.Conduit
    Description : Phase vocoder in Conduit
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module wraps phase vocoder algorithms for use in Conduit.
Two variants are provided, one for single channel processing,
and another for processing multiple channels synchronously.
-}
module Vocoder.Conduit(
      -- * Single-channel functions
      volumeFix,
      analysis,
      synthesis,
      processFrames,
      process,
      -- * Multi-channel functions
      volumeFixF,
      analysisF,
      synthesisF,
      processFramesF
    ) where

import Data.Conduit
import qualified Data.Conduit.List as DCL
import qualified Data.List.NonEmpty as DLN
import qualified Data.Vector.Storable as V
import Control.Arrow
import Vocoder
import Vocoder.Conduit.Frames

-- | Corrects for volume change introduced by STFT processing.
volumeFix :: Monad m => VocoderParams -> ConduitT STFTFrame STFTFrame m ()
volumeFix par = DCL.map $ V.map (* volumeCoeff par) *** id

-- | Perform the phase vocoder analysis phase.
analysis :: Monad m => VocoderParams -> Phase -> ConduitT Frame STFTFrame m Phase
analysis par ph = DCL.mapAccum (flip $ analysisBlock par) ph

-- | Perform the phase vocoder synthesis phase.
synthesis :: Monad m => VocoderParams -> Phase -> ConduitT STFTFrame Frame m Phase
synthesis par ph = DCL.mapAccum (flip $ synthesisBlock par) ph

-- | Perform frequency domain processing on overlapping frames.
processFrames :: Monad m => VocoderParams -> (Phase, Phase) -> ConduitT STFTFrame STFTFrame m r -> ConduitT Frame Frame m (r, (Phase, Phase))
processFrames par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysis par p1 `fuseBoth` (volumeFix par .| c) `fuseBoth` synthesis par p2

-- | Perform frequency domain processing on a chunked stream.
process :: Monad m => VocoderParams -> ConduitT STFTFrame STFTFrame m r -> ConduitT Frame Frame m r
process par c0 = (\(r, _, _) -> r) <$> processWith V.empty (zeroPhase par, zeroPhase par) c0
    where
    processWith q phs c = (\(q', (r, ph)) -> (r, q', ph)) <$>  (genFramesOfE (vocInputFrameLength par) (vocHopSize par) q `fuseBoth` processFrames par phs c) `fuseUpstream` sumFramesE (vocHopSize par) (vocHopSize par)

app_help :: Applicative f => (a -> s -> (s, b)) -> f a -> f s -> (f s, f b)
app_help f a b = DLN.unzip $ fmap (uncurry f) ((,) <$> a <*> b)

-- | Corrects for volume change introduced by STFT processing. 
volumeFixF :: (Applicative f, Monad m) => VocoderParams -> ConduitT (f STFTFrame) (f STFTFrame) m ()
volumeFixF par = DCL.map $ fmap $ V.map (* volumeCoeff par) *** id

-- | Perform the phase vocoder analysis phase.
analysisF :: (Applicative f, Monad m) => VocoderParams -> f Phase -> ConduitT (f Frame) (f STFTFrame) m (f Phase)
analysisF par ph = DCL.mapAccum (app_help $ flip $ analysisBlock par) ph

-- | Perform the phase vocoder synthesis phase.
synthesisF :: (Applicative f, Monad m) => VocoderParams -> f Phase -> ConduitT (f STFTFrame) (f Frame) m (f Phase)
synthesisF par ph = DCL.mapAccum (app_help $ flip $ synthesisBlock par) ph

-- | Perform frequency domain processing on overlapping frames.
processFramesF :: (Applicative f, Monad m) => VocoderParams -> (f Phase, f Phase) -> ConduitT (f STFTFrame) (f STFTFrame) m r -> ConduitT (f Frame) (f Frame) m (r, (f Phase, f Phase))
processFramesF par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysisF par p1 `fuseBoth` (volumeFixF par .| c) `fuseBoth` synthesisF par p2


